(ns swetc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :as shell]
            [cmake-parser.core :as cmake-parser])
  (:import [org.apache.commons.io FilenameUtils]
           [javax.xml.xpath XPathFactory XPath XPathConstants]
           [javax.xml.parsers DocumentBuilderFactory])
  (:gen-class))


(defmulti line-count-of-file
  "return line count of file."
  class)

(defmethod line-count-of-file java.io.File
  [file-obj]
  (try
    (with-open [rdr (io/reader file-obj)]
      (count (line-seq rdr)))
    (catch Exception e 0)))

(defmethod line-count-of-file String
  [file-path]
  (line-count-of-file (io/file file-path)))

(defn line-count-of-files
  [files]
  (reduce + (map line-count-of-file files)))

(defmulti glob-by-file-exts
  "glob by file extensions."
  (fn [dir exts]
    (class dir)))

(defmethod glob-by-file-exts java.io.File
  [dir-obj exts]
  (let [files (filter #(.isFile %) (file-seq dir-obj))]
    (for [file files
          :let [ext (FilenameUtils/getExtension (.getName file))]
          :when (some #(.equalsIgnoreCase ext %) exts)]
      file)))

(defmethod glob-by-file-exts String
  [dir-path exts]
  (glob-by-file-exts (io/file dir-path) exts))

(defmulti glob-by-file-name
  "glob by file name wildcard or regex pattern."
  (fn [dir file-name]
    [(class dir) (class file-name)]))

(defmethod glob-by-file-name [java.io.File String]
  [dir-obj wildcard]
  (let [files (filter #(.isFile %) (file-seq dir-obj))]
    (filter #(-> %
                 .getName
                 (FilenameUtils/wildcardMatchOnSystem wildcard))
            files)))

(defmethod glob-by-file-name [java.io.File java.util.regex.Pattern]
  [dir-obj regex]
  (let [files (filter #(.isFile %) (file-seq dir-obj))]
    (filter #(->> %
                 .getName
                 (re-matches regex))
            files)))

(defmethod glob-by-file-name [String String]
  [dir-path wildcard]
  (glob-by-file-name (io/file dir-path) wildcard))

(defmethod glob-by-file-name [String java.util.regex.Pattern]
  [dir-path regex]
  (glob-by-file-name (io/file dir-path) regex))

(defn cmake-files
  [dir-path]
  (glob-by-file-name dir-path "CMakeLists.txt"))

(defn vcxproj-files
  [dir-path]
  (glob-by-file-exts dir-path #{"vcxproj"}))

(defn csproj-files
  [dir-path]
  (glob-by-file-exts dir-path #{"csproj"}))

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
  (shell/sh "tf" "checkout" file-path))

(defn tf-rename
  [from to]
  (shell/sh "tf" "rename" from to))

(defn tf-undo
  [file-path]
  (shell/sh "tf" "undo" "/noprompt" file-path))

(def cmdlets {})
(defmacro defcmdlet
  [cmd help & body]
  `(alter-var-root (var cmdlets) #(assoc %1 %2 %3) '~cmd
                   {:help ~help, :fn (fn ~@body)}))

(defcmdlet line-count
  "<file-path>  --  line count of file.
    <dir-path> [ext1 ext2 ...]  --  line count of files in directory (glob by file extensions)."
  [path & exts]
  (let [obj (io/file path) exts (set exts)]
    (if (.isDirectory obj)
      (println (line-count-of-files (glob-by-file-exts obj exts)))
      (if (.isFile obj)
        (println (line-count-of-file obj))
        (println "not a file or directory.")))))

(defcmdlet parse-csproj
  "<dir-path> [<P1> <V1> <P2> <V2> ...]  --  parse *.csproj in directory."
  [dir-path & props]
  (doseq [file (csproj-files dir-path)
          :let [proj-path (.getPath file)
                res (try
                      (apply parse-csproj-file file props)
                      (catch Exception e (println "***error parsing:" proj-path)))]
          :when res]
    (println proj-path)
    (println "TargetName:" (:TargetName res))
    (println "TargetType:" (:TargetType res))
    (println "Dependencies:")
    (doseq [dep (:Dependencies res)]
      (println "   " dep))))

(defcmdlet parse-vcxproj
  "<dir-path> [<P1> <V1> <P2> <V2> ...]  --  parse *.vcxproj in directory."
  [dir-path & props]
  (doseq [file (vcxproj-files dir-path)
          :let [proj-path (.getPath file)
                res (try
                      (apply parse-vcxproj-file file props)
                      (catch Exception e (println "***error parsing:" proj-path)))]
          :when res]
    (println proj-path)
    (println "TargetName:" (:TargetName res))
    (println "TargetType:" (:TargetType res))
    (println "Dependencies:")
    (doseq [dep (:Dependencies res)]
      (println "   " dep))))

(defcmdlet parse-cmake
  "<dir-path>  --  parse CMakeLists.txt in directory."
  [dir-path]
  (doseq [file (cmake-files dir-path)
          :let [proj-path (.getPath file)
                res (try
                      (parse-cmake-file file)
                      (catch Exception e (println "***error parsing:" proj-path)))]
          :when res]
    (println proj-path)
    (println "TargetName:" (:TargetName res))
    (println "TargetType:" (:TargetType res))
    (println "Dependencies:")
    (doseq [dep (:Dependencies res)]
      (println "   " dep))))

(defcmdlet help
  "print this help."
  []
  (println "Available tools:")
  (doseq [[cmd {help :help}] cmdlets]
    (println cmd)
    (println "   " help)))

(defn -main
  [& args]
  (if (or (< (count args) 1)
          (not (get cmdlets (symbol (first args)))))
    ((:fn (get cmdlets 'help)))
    (apply (:fn (get cmdlets (symbol (first args)))) (nthrest args 1)))
  (shutdown-agents))

