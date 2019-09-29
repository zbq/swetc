(ns swetc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :as shell]
            [cmake-parser.core :as cmake-parser])
  (:import [org.apache.commons.io FilenameUtils]
           [javax.xml.xpath XPathFactory XPath XPathConstants]
           [javax.xml.parsers DocumentBuilderFactory]
           [java.io IOException])
  (:gen-class))


(defmulti line-count-of-file
  "return line count of file."
  class)

(defmethod line-count-of-file java.io.File
  [file-obj]
  (try
    (with-open [rdr (io/reader file-obj)]
      (count (line-seq rdr)))
    (catch Exception _ 0)))

(defmethod line-count-of-file String
  [file-path]
  (line-count-of-file (io/file file-path)))

(defn line-count-of-files
  [files]
  (reduce + (map line-count-of-file files)))

(defn- list-files
  [recursive dir-obj]
  (filter #(.isFile %)
          (if recursive
            (file-seq dir-obj)
            (.listFiles dir-obj))))

(defmulti glob-by-file-ext
  "glob by file extension."
  (fn [recursive dir exts]
    (class dir)))

(defmethod glob-by-file-ext java.io.File
  [recursive dir-obj exts]
  (for [file (list-files recursive dir-obj)
        :let [ext (FilenameUtils/getExtension (.getName file))]
        :when (some #(.equalsIgnoreCase ext %) exts)]
    file))

(defmethod glob-by-file-ext String
  [recursive dir-path exts]
  (glob-by-file-ext recursive (io/file dir-path) exts))

(defn- file-name-match?
  [file-name matcher]
  (if (instance? String matcher)
    ;; wildcard
    (FilenameUtils/wildcardMatchOnSystem file-name matcher)
    ;; regex pattern
    (re-matches matcher file-name)))

(defmulti glob-by-file-name
  "glob by file name wildcard or regex pattern."
  (fn [recursive dir matchers]
    (class dir)))

(defmethod glob-by-file-name java.io.File
  [recursive dir-obj matchers]
  (for [file (list-files recursive dir-obj)
        :let [file-name (.getName file)]
        :when (some #(file-name-match? file-name %) matchers)]
    file))

(defmethod glob-by-file-name String
  [recursive dir-path matchers]
  (glob-by-file-name recursive (io/file dir-path) matchers))

(defn cmake-files
  [dir-path]
  (glob-by-file-name true dir-path #{"CMakeLists.txt"}))

(defn vcxproj-files
  [dir-path]
  (glob-by-file-ext true dir-path #{"vcxproj"}))

(defn csproj-files
  [dir-path]
  (glob-by-file-ext true dir-path #{"csproj"}))

(defn- xml-doc-from-file
  [file-path]
  (let [dbf (DocumentBuilderFactory/newDefaultInstance)
        db (.newDocumentBuilder dbf)]
    (.parse db file-path)))

(defn- xml-doc-from-string
  [content]
  (let [dbf (DocumentBuilderFactory/newDefaultInstance)
        db (.newDocumentBuilder dbf)]
    (.parse db (java.io.StringBufferInputStream. content))))

(defn- eval-xpath
  "return node list."
  [xml-doc expr]
  (let [xp (.newXPath (XPathFactory/newDefaultInstance))
        nodes (.evaluate xp expr xml-doc XPathConstants/NODESET)]
    (for [i (range (.getLength nodes))]
      (.item nodes i))))

(defmacro only-one-node
  [form]
  `(let [nodes# ~form]
     (do
       (assert (== (count nodes#) 1) (str "find more than one node by " '~form))
       (first nodes#))))

(defn- parse-proj-file
  "return a map with keys: TargetName, TargetType(staticlibrary/library/exe), Dependencies."
  [file-path hook & props]
  (let [content (slurp file-path)
        index (string/last-index-of content "</Project>")
        tmp-proj-file-path (str file-path "-swetc-tmp")
        tmp-content (str (subs content 0 index) hook "</Project>")
        _ (spit tmp-proj-file-path tmp-content)
        res (try
              (apply shell/sh "msbuild" tmp-proj-file-path
                     "/nologo" "/v:minimal" "/t:SWETC-PARSE"
                     (for [[name val] (apply hash-map props)]
                       (str "/p:" name "=" val)))
              (catch IOException e (do
                                     (println "No msbuild found!")
                                     (throw e)))
              (finally (io/delete-file tmp-proj-file-path true)))
        xml-doc (xml-doc-from-string (:out res))
        target-name (.getTextContent
                     (only-one-node
                      (eval-xpath xml-doc "//SWETC/TargetName")))
        target-type (.getTextContent
                     (only-one-node
                      (eval-xpath xml-doc "//SWETC/TargetType")))
        deps (string/split
              (.getTextContent (only-one-node
                                (eval-xpath xml-doc "//SWETC/Dependencies")))
              #"[; \n]")
        deps (map #(string/trim %) deps)
        deps (apply sorted-set
                    (map #(if (string/ends-with? % ".lib")
                            (subs % 0 (- (.length %) 4))
                            %) deps))]
    {:TargetName target-name
     :TargetType target-type
     :Dependencies deps}))

(defn parse-vcxproj-file
  [file-path & props]
  (apply parse-proj-file file-path "
<ItemGroup>
    <Link Include='Whatever' />
</ItemGroup>
<Target Name='SWETC-PARSE'>
    <Message Text='&lt;SWETC&gt;' Importance='High' />
    <Message Text='&lt;TargetName&gt;$(TargetName)&lt;/TargetName&gt;' Importance='High' />
    <Message Text='&lt;TargetType&gt;$(OutputType)&lt;/TargetType&gt;' Importance='High' />
    <Message Text='&lt;Dependencies&gt;%(Link.AdditionalDependencies)&lt;/Dependencies&gt;' Importance='High' />
    <Message Text='&lt;/SWETC&gt;' Importance='High' />
</Target>
" "Configuration" "Release" "Platform" "x64" props))

(defn parse-csproj-file
  [file-path & props]
  (let [res (apply parse-proj-file file-path "
<Target Name='SWETC-PARSE'>
    <Message Text='&lt;SWETC&gt;' Importance='High' />
    <Message Text='&lt;TargetName&gt;$(TargetName)&lt;/TargetName&gt;' Importance='High' />
    <Message Text='&lt;TargetType&gt;$(OutputType)&lt;/TargetType&gt;' Importance='High' />
    <Message Text='&lt;Dependencies&gt;@(Reference)&lt;/Dependencies&gt;' Importance='High' />
    <Message Text='&lt;/SWETC&gt;' Importance='High' />
</Target>
" "Configuration" "Release" "Platform" "AnyCPU" props)
        target-type (string/lower-case (:TargetType res))
        target-type (if (= target-type "winexe")
                      "exe"
                      target-type)
        deps (:Dependencies res)
        ;; handle something like: Microsoft.Expression.Drawing, Version=4.0.0.0, Culture=neutral...
        deps (apply sorted-set (map #(string/trim (first (string/split % #"[,]"))) deps))]
    (assoc res :TargetType target-type :Dependencies deps)))

(defn parse-cmake-file
  [file-path]
  (let [invocs (cmake-parser/parse-file file-path)
        bindings (transient {})
        target-name (atom "")
        target-type (atom "")
        deps (transient [])]
    (doseq [invoc invocs]
      (cond
        (.equalsIgnoreCase "SET" (first invoc))
        (assoc! bindings (second invoc)
                (cmake-parser/expand-argument (nth invoc 2 "") bindings)) ;; SET(xxx )
        (.equalsIgnoreCase "ADD_EXECUTABLE" (first invoc))
        (do (reset! target-type "exe")
            (reset! target-name (cmake-parser/expand-argument (second invoc) bindings)))
        (.equalsIgnoreCase "ADD_LIBRARY" (first invoc))
        (do (if (some #(.equalsIgnoreCase "SHARED" %) invoc)
              (reset! target-type "library")
              (reset! target-type "staticlibrary"))
            (reset! target-name (cmake-parser/expand-argument (second invoc) bindings)))
        (.equalsIgnoreCase "TARGET_LINK_LIBRARIES" (first invoc))
        (conj! deps (nthrest invoc 2))))
    {:TargetName @target-name
     :TargetType @target-type
     :Dependencies (-> (apply sorted-set
                              (map #(if (and (string/starts-with? % "lib")
                                             (string/ends-with? % ".so"))
                                      (subs % 3 (- (.length %) 3))
                                      %)
                                   (flatten (persistent! deps))))
                       (disj "PRIVATE")
                       (disj "PUBLIC")
                       (disj "INTERFACE"))}
    ))

(defn tf-checkout
  [file-path]
  (try
    (shell/sh "tf" "checkout" file-path)
    (catch IOException e (do
                           (println "No tf found!")
                           (throw e)))))

(defn tf-rename
  [from to]
  (try
    (shell/sh "tf" "rename" from to)
    (catch IOException e (do
                           (println "No tf found!")
                           (throw e)))))

(defn tf-undo
  [file-path]
  (try
    (shell/sh "tf" "undo" "/noprompt" file-path)
    (catch IOException e (do
                           (println "No tf found!")
                           (throw e)))))

(defn line-count
  [& args]
  (when (or (< (count args) 2)
            (and (= "-r" (first args)) (< (count args) 3)))
    (throw (java.lang.IllegalArgumentException.)))
  (let [recursive (= "-r" (first args))
        args (if recursive (rest args) args)
        [dir-path & wildcards] args]
    (println (line-count-of-files (glob-by-file-name recursive dir-path wildcards)))))

(defn parse-csproj
  [dir-path & props]
  (doseq [file (csproj-files dir-path)
          :let [proj-path (.getPath file)
                res (try
                      (apply parse-csproj-file file props)
                      (catch Exception _ (println "Error parsing:" proj-path)))]
          :when res]
    (println proj-path)
    (println "TargetName:" (:TargetName res))
    (println "TargetType:" (:TargetType res))
    (println "Dependencies:")
    (doseq [dep (:Dependencies res)]
      (println "   " dep))))

(defn parse-vcxproj
  [dir-path & props]
  (doseq [file (vcxproj-files dir-path)
          :let [proj-path (.getPath file)
                res (try
                      (apply parse-vcxproj-file file props)
                      (catch Exception _ (println "Error parsing:" proj-path)))]
          :when res]
    (println proj-path)
    (println "TargetName:" (:TargetName res))
    (println "TargetType:" (:TargetType res))
    (println "Dependencies:")
    (doseq [dep (:Dependencies res)]
      (println "   " dep))))

(defn parse-cmake
  [dir-path]
  (doseq [file (cmake-files dir-path)
          :let [proj-path (.getPath file)
                res (try
                      (parse-cmake-file file)
                      (catch Exception _ (println "Error parsing:" proj-path)))]
          :when res]
    (println proj-path)
    (println "TargetName:" (:TargetName res))
    (println "TargetType:" (:TargetType res))
    (println "Dependencies:")
    (doseq [dep (:Dependencies res)]
      (println "   " dep))))

(declare cmdlets)

(defn help
  ([]
   (println "Available tools:")
   (doseq [[name {brief :brief}] cmdlets]
     (println name " -- " brief)))
  ([name]
   (if-let [cmdlet (cmdlets name)]
     (do
       (println (:brief cmdlet))
       (println (:usage cmdlet)))
     (println "Unknown tool"))))

(def cmdlets
  {"line-count"
   {:brief "Line count of files in directory."
    :usage "Usage: line-count [-r] <dir-path> <wildcard1> [wildcard2 ...]  --  recursively if -r specified."
    :fn line-count}
   "parse-csproj"
   {:brief "Parse C# project files(*.csproj) in directory."
    :usage "Usage: parse-csproj <dir-path> [Property Value ...]  --  you can provide additional property and value such as Platform AnyCPU."
    :fn parse-csproj}
   "parse-vcxproj"
   {:brief "Parse C++ project files(*.vcxproj) in directory."
    :usage "Usage: parse-vcxproj <dir-path> [Property Value ...]  --  you can provide additional property and value such as Platform x64."
    :fn parse-vcxproj}
   "parse-cmake"
   {:brief "Parse CMakeLists.txt files in directory."
    :usage "Usage: parse-cmake <dir-path>"
    :fn parse-cmake}
   "help"
   {:brief "Print this help."
    :usage "Usage: help [tool]"
    :fn help}
   })

(defn -main
  [& args]
  (if (or (== (count args) 0)
          (not (cmdlets (first args))))
    ;; if no tool provided or tool not found, print help
    (help)
    (try
      (apply (:fn (cmdlets (first args))) (nthrest args 1))
      (catch java.lang.IllegalArgumentException _
        (help (first args)))))
  (shutdown-agents))

