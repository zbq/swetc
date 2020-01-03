(ns swetc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :as shell]
            [cmake-parser.core :as cmake-parser])
  (:import [org.apache.commons.io FilenameUtils]
           [javax.xml.transform TransformerFactory]
           [javax.xml.transform.stream StreamSource]
           [org.dom4j.io SAXReader OutputFormat XMLWriter DocumentSource DocumentResult]
           [java.io IOException FileWriter]
           [java.nio.file Files FileVisitOption Path Paths])
  (:gen-class))


(defn path-of
  [first & rest]
  (.normalize (Paths/get (str first) (into-array String (map str rest)))))

(defn path-to-absolute
  [path]
  (.toAbsolutePath path))

(defn path-parent
  [path]
  (.getParent path))

(defn path-file-name
  [path]
  (.toString (.getFileName path)))

(defn path-exists
  [path]
  (.exists (.toFile path)))

(defn path-is-file
  [path]
  (.isFile (.toFile path)))

(defn path-is-dir
  [path]
  (.isDirectory (.toFile path)))

(defn path-files
  [dir-path recursive]
  (->> (if recursive
         (Files/walk dir-path (into-array FileVisitOption (list FileVisitOption/FOLLOW_LINKS)))
         (Files/list dir-path))
       (.iterator)
       (iterator-seq)
       (filter #(path-is-file %))))

(defn map-of-entries
  [entries]
  (loop [m {} entries entries]
    (if (seq entries)
      (recur (assoc m (key (first entries)) (val (first entries)))
             (next entries))
      m)))

(defn line-count-of-file
  [file-path]
  (try
    (with-open [rdr (io/reader (.toFile file-path))]
      (count (line-seq rdr)))
    (catch Exception _ 0)))

(defn line-count-of-files
  [file-paths]
  (reduce + (map line-count-of-file file-paths)))

(defn glob-by-file-ext
  "glob by file extension, return Path list."
  [dir-path exts recursive]
  (for [file-path (path-files dir-path recursive)
        :let [ext (FilenameUtils/getExtension (path-file-name file-path))]
        :when (some #(.equalsIgnoreCase ext %) exts)]
    file-path))

(defn- file-name-match?
  [file-name matcher]
  (if (instance? String matcher)
    ;; wildcard
    (FilenameUtils/wildcardMatchOnSystem file-name matcher)
    ;; regex pattern
    (re-matches matcher file-name)))

(defn glob-by-file-name
  "glob by file name wildcard or regex pattern, return Path list."
  [dir-path matchers recursive]
  (for [file-path (path-files dir-path recursive)
        :let [file-name (path-file-name file-path)]
        :when (some #(file-name-match? file-name %) matchers)]
    file-path))

(defn cmake-files
  [dir-path]
  (glob-by-file-name dir-path #{"CMakeLists.txt"} true))

(defn vcxproj-files
  [dir-path]
  (glob-by-file-ext dir-path #{"vcxproj"} true))

(defn csproj-files
  [dir-path]
  (glob-by-file-ext dir-path #{"csproj"} true))

(defn sln-files
  [dir-path]
  (glob-by-file-ext dir-path #{"sln"} true))

(defn projs-of-sln
  [sln-path]
  (let [dir (path-parent sln-path)]
    (filter #(path-is-file %)
            (map #(path-of dir (second %))
                 (re-seq #"Project[^,]+,[^\"]*\"([^\"]+)\""
                         (slurp (.toFile sln-path)))))))

(defn xml-doc-from-file
  [xml-path]
  (let [rdr (SAXReader.)]
    (.read rdr (.toFile xml-path))))

(defn xml-doc-from-string
  [content]
  (let [rdr (SAXReader.)]
    (.read rdr (java.io.StringBufferInputStream. content))))

(defn xml-doc-to-file
  [xml-doc xml-path]
  (with-open [writer (XMLWriter. (FileWriter. (str xml-path)) (OutputFormat/createPrettyPrint))]
    (.write writer xml-doc)))

(defn xmlns-of-node
  [node]
  (.getNamespaceURI node))

(defn xpath-select-one
  [node xpath-expr & [xpath-ns-map]]
  (let [xpath (.createXPath node xpath-expr)]
    (when xpath-ns-map
      (.setNamespaceURIs xpath xpath-ns-map))
    (.selectSingleNode xpath node)))

(defn xpath-select
  "return node list."
  [node xpath-expr & [xpath-ns-map]]
  (let [xpath (.createXPath node xpath-expr)]
    (when xpath-ns-map
      (.setNamespaceURIs xpath xpath-ns-map))
    (.selectNodes xpath node)))

(defn xslt-transform
  [xml-doc xslt-path]
  (let [factory (TransformerFactory/newInstance)
        trans (.newTransformer factory (StreamSource. (str xslt-path)))
        result (DocumentResult.)]
    (.transform trans (DocumentSource. xml-doc) result)
    (.getDocument result)))

(defn- vsproj-default-props
  [proj-path]
  (let [xml-doc (xml-doc-from-file proj-path)
        xmlns (xmlns-of-node (.getRootElement xml-doc))
        conf_node (xpath-select-one xml-doc "/n:Project/n:PropertyGroup/n:Configuration" {"n" xmlns})
        conf (if conf_node (.getText conf_node) "Release")
        platform_node (xpath-select-one xml-doc "/n:Project/n:PropertyGroup/n:Platform" {"n" xmlns})
        platform (if platform_node (.getText platform_node) "x64")]
    (list "Configuration" conf "Platform" platform)))

;; vs => visual studio
(defn- parse-vsproj-file
  "return a map with keys: TargetName, TargetType(staticlibrary/library/exe), Dependencies."
  [proj-path hook & props]
  (let [props (concat (vsproj-default-props proj-path) props)
        content (slurp (.toFile proj-path))
        index (string/last-index-of content "</Project>")
        tmp-proj-file-path (str proj-path "-swetc-tmp")
        tmp-content (str (subs content 0 index) hook "</Project>")
        _ (spit tmp-proj-file-path tmp-content)
        res (try
              (apply shell/sh "msbuild" tmp-proj-file-path
                     "/nologo" "/v:minimal" "/t:SWETC-PARSE"
                     (for [[name val] (apply hash-map props)]
                       (str "/p:" name "=" val)))
              (catch IOException e
                (println "No msbuild found!")
                (throw e))
              (finally (io/delete-file tmp-proj-file-path true)))
        res (:out res)
        res (subs res (string/index-of res "<SWETC>")
                  (+ (string/index-of res "</SWETC>") 8))
        xml-doc (xml-doc-from-string res)
        target-name (.getText
                     (xpath-select-one xml-doc "//SWETC/TargetName"))
        target-type (.getText
                     (xpath-select-one xml-doc "//SWETC/TargetType"))
        deps (string/split
              (.getText (xpath-select-one xml-doc "//SWETC/Dependencies"))
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
  [proj-path & props]
  (apply parse-vsproj-file proj-path "
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
" "Configuration" "Release" props))

(defn parse-csproj-file
  [proj-path & props]
  (let [res (apply parse-vsproj-file proj-path "
<Target Name='SWETC-PARSE'>
    <Message Text='&lt;SWETC&gt;' Importance='High' />
    <Message Text='&lt;TargetName&gt;$(TargetName)&lt;/TargetName&gt;' Importance='High' />
    <Message Text='&lt;TargetType&gt;$(OutputType)&lt;/TargetType&gt;' Importance='High' />
    <Message Text='&lt;Dependencies&gt;@(Reference)&lt;/Dependencies&gt;' Importance='High' />
    <Message Text='&lt;/SWETC&gt;' Importance='High' />
</Target>
" "Configuration" "Release" props)
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
  (let [invocs (cmake-parser/parse-file (str file-path))
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

(defn parse-vcxproj-files
  [proj-paths & props]
  (map-of-entries
   (for [proj-path proj-paths
         :let [info (try
                      (apply parse-vcxproj-file proj-path props)
                      (catch Exception e
                        (println "Error parsing:" (str proj-path))
                        (println e)))]
         :when info]
     (clojure.lang.MapEntry. (str proj-path) info))))

(defn parse-csproj-files
  [proj-paths & props]
  (map-of-entries
   (for [proj-path proj-paths
         :let [info (try
                      (apply parse-csproj-file proj-path props)
                      (catch Exception e
                        (println "Error parsing:" (str proj-path))
                        (println e)))]
         :when info]
     (clojure.lang.MapEntry. (str proj-path) info))))

(defn parse-cmake-files
  [file-paths]
  (map-of-entries
   (for [file-path file-paths
         :let [info (try
                      (parse-cmake-file file-path)
                      (catch Exception e
                        (println "Error parsing:" (str file-path))
                        (println e)))]
         :when info]
     (clojure.lang.MapEntry. (str file-path) info))))

(defn map-target->proj
  [proj->info]
  (map-of-entries
   (for [[proj info] proj->info]
     (clojure.lang.MapEntry. (:TargetName info) proj))))

(defn map-proj->direct-dep-projs
  [proj->info target->proj]
  (map-of-entries
   (for [[proj info] proj->info]
     (clojure.lang.MapEntry. proj (distinct (remove nil? (map target->proj (:Dependencies info))))))))

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
  [file]
  (try
    (shell/sh "tf" "add" file)
    (catch IOException e
      (println "No tf found!")
      (throw e))))

(defn tf-checkout
  [file]
  (try
    (shell/sh "tf" "checkout" file)
    (catch IOException e
      (println "No tf found!")
      (throw e))))

(defn tf-rename
  [from to]
  (try
    (shell/sh "tf" "rename" from to)
    (catch IOException e
      (println "No tf found!")
      (throw e))))

(defn tf-undo
  [file]
  (try
    (shell/sh "tf" "undo" "/noprompt" file)
    (catch IOException e
      (println "No tf found!")
      (throw e))))

(defn line-count
  [& args]
  (when (or (< (count args) 2)
            (and (= "-r" (first args)) (< (count args) 3)))
    (throw (java.lang.IllegalArgumentException.)))
  (let [recursive (= "-r" (first args))
        args (if recursive (rest args) args)
        [dir & wildcards] args]
    (println (line-count-of-files (glob-by-file-name (path-of dir) wildcards recursive)))))

(defn parse-csproj-in-dir
  [dir & props]
  (doseq [[proj info] (apply parse-csproj-files (csproj-files (path-of dir)) props)]
    (println proj)
    (println "TargetName:" (:TargetName info))
    (println "TargetType:" (:TargetType info))
    (println "Dependencies:")
    (doseq [dep (:Dependencies info)]
      (println "   " dep))))

(defn parse-vcxproj-in-dir
  [dir & props]
  (doseq [[proj info] (apply parse-vcxproj-files (vcxproj-files (path-of dir)) props)]
    (println proj)
    (println "TargetName:" (:TargetName info))
    (println "TargetType:" (:TargetType info))
    (println "Dependencies:")
    (doseq [dep (:Dependencies info)]
      (println "   " dep))))

(defn parse-cmake-in-dir
  [dir]
  (doseq [[proj info] (parse-cmake-files (cmake-files (path-of dir)))]
    (println proj)
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

