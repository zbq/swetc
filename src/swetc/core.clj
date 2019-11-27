(ns swetc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :as shell]
            [cmake-parser.core :as cmake-parser])
  (:import [org.apache.commons.io FilenameUtils]
           [javax.xml.xpath XPathFactory XPath XPathConstants]
           [javax.xml.parsers DocumentBuilderFactory]
           [javax.xml.transform TransformerFactory]
           [javax.xml.transform.dom DOMSource]
           [javax.xml.transform.stream StreamSource StreamResult]
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

(defn xml-doc-from-file
  [file-path]
  (let [dbf (DocumentBuilderFactory/newDefaultInstance)
        db (.newDocumentBuilder dbf)]
    (.parse db file-path)))

(defn xml-doc-from-string
  [content]
  (let [dbf (DocumentBuilderFactory/newDefaultInstance)
        db (.newDocumentBuilder dbf)]
    (.parse db (java.io.StringBufferInputStream. content))))

(defn xml-doc-to-string
  [xml-doc]
  (let [factory (TransformerFactory/newInstance)
        trans (.newTransformer factory)
        dom (DOMSource. xml-doc)]
    (with-out-str (.transform trans dom (StreamResult. *out*)))))

(defn eval-xpath
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

(defn- string-input-stream
  [s]
  (-> (java.io.StringReader. s) clojure.lang.LineNumberingPushbackReader.))

(defn xslt-transform
  [xslt-str xml-str]
  (let [factory (TransformerFactory/newInstance)
        templates (.newTemplates factory (StreamSource.
                                          (string-input-stream xslt-str)))
        trans (.newTransformer templates)
        source (StreamSource. (string-input-stream xml-str))
        out (java.io.StringWriter.)
        result (StreamResult. out)]
    (.transform trans source result)
    (str out)))

;; vs => visual studio
(defn- parse-vsproj-file
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
        res (:out res)
        res (subs res (string/index-of res "<SWETC>")
                  (+ (string/index-of res "</SWETC>") 8))
        xml-doc (xml-doc-from-string res)
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
  (apply parse-vsproj-file file-path "
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
  (let [res (apply parse-vsproj-file file-path "
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

(defn path-normalize
  [path]
  (.getCanonicalPath (io/file path)))

(defn path-file-name
  [path]
  (.getName (io/file path)))

(defn parse-vcxproj-files
  [file-paths & props]
  (apply hash-map
         (flatten (for [file-path (distinct
                                   (for [f file-paths]
                                     (path-normalize f)))
                        :let [info (try
                                     (apply parse-vcxproj-file file-path props)
                                     (catch Exception _ (println "Error parsing:" file-path)))]
                        :when info]
                    (list file-path info)))))

(defn parse-csproj-files
  [file-paths & props]
  (apply hash-map
         (flatten (for [file-path (distinct
                                   (for [f file-paths]
                                     (path-normalize f)))
                        :let [info (try
                                     (apply parse-csproj-file file-path props)
                                     (catch Exception _ (println "Error parsing:" file-path)))]
                        :when info]
                    (list file-path info)))))

(defn parse-cmake-files
  [file-paths]
  (apply hash-map
         (flatten (for [file-path (distinct
                                   (for [f file-paths]
                                     (path-normalize f)))
                        :let [info (try
                                     (apply parse-cmake-file file-path)
                                     (catch Exception _ (println "Error parsing:" file-path)))]
                        :when info]
                    (list file-path info)))))

(defn map-target->proj
  [proj->info]
  (apply hash-map
         (flatten (for [[proj info] proj->info]
                    (list (:TargetName info) proj)))))

(defn map-proj->direct-dep-projs
  [proj->info target->proj]
  (apply hash-map
         (flatten (for [[proj info] proj->info]
                    (list proj (set (filter identity (map target->proj (:Dependencies info)))))))))

(defn- find-cyclic-dep1
  [dep-stack dep]
  (loop [i (dec (count dep-stack))]
    (when (>= i 0)
      (if (= (nth dep-stack i) dep)
        (atom (subvec dep-stack i))
        (recur (dec i))))))

(defn- find-cyclic-dep2
  [dep-stack k->direct-deps]
  (for [dep (k->direct-deps (last dep-stack))]
    (if-let [cyc (find-cyclic-dep1 dep-stack dep)]
      cyc
      (find-cyclic-dep2 (conj dep-stack dep) k->direct-deps))))

;; {a #{b}, b #{c}, c #{a}} => ([a b c] [b c a] [c a b])
;; [a b c] => (a b c a) => ((a b) (b c) (c a)) => (a b b c c a) => {a b, b c, c a}
;; => {{a b, b c, c a} [[a b c] [b c a] [c a b]]} => [a b c]
(defn find-cyclic-dependencies
  [k->direct-deps]
  (let [cycs (map #(find-cyclic-dep2 [%] k->direct-deps) (keys k->direct-deps))]
    (map first
         (vals
          (group-by #(apply hash-map (flatten (partition 2 1 (take (inc (count %)) (cycle %)))))
                    (map deref
                         (filter identity
                                 (flatten cycs))))))))

(defn tf-add
  [file-path]
  (try
    (shell/sh "tf" "add" file-path)
    (catch IOException e (do
                           (println "No tf found!")
                           (throw e)))))

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

(defn parse-csproj-in-dir
  [dir-path & props]
  (doseq [[file-path info] (apply parse-csproj-files (csproj-files dir-path) props)]
    (println file-path)
    (println "TargetName:" (:TargetName info))
    (println "TargetType:" (:TargetType info))
    (println "Dependencies:")
    (doseq [dep (:Dependencies info)]
      (println "   " dep))))

(defn parse-vcxproj-in-dir
  [dir-path & props]
  (doseq [[file-path info] (apply parse-vcxproj-files (vcxproj-files dir-path) props)]
    (println file-path)
    (println "TargetName:" (:TargetName info))
    (println "TargetType:" (:TargetType info))
    (println "Dependencies:")
    (doseq [dep (:Dependencies info)]
      (println "   " dep))))

(defn parse-cmake-in-dir
  [dir-path]
  (doseq [[file-path info] (parse-cmake-files (cmake-files dir-path))]
    (println file-path)
    (println "TargetName:" (:TargetName info))
    (println "TargetType:" (:TargetType info))
    (println "Dependencies:")
    (doseq [dep (:Dependencies info)]
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
    :fn parse-csproj-in-dir}
   "parse-vcxproj"
   {:brief "Parse C++ project files(*.vcxproj) in directory."
    :usage "Usage: parse-vcxproj <dir-path> [Property Value ...]  --  you can provide additional property and value such as Platform x64."
    :fn parse-vcxproj-in-dir}
   "parse-cmake"
   {:brief "Parse CMakeLists.txt files in directory."
    :usage "Usage: parse-cmake <dir-path>"
    :fn parse-cmake-in-dir}
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

