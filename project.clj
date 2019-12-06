(defproject swetc "0.1.0"
  :description "Software Engineering Tool Collection"
  :url "http://github.com/zbq/swetc.git"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [cmake-parser/cmake-parser "0.1.0"]
                 [commons-io/commons-io "2.6"]
                 [org.dom4j/dom4j "2.1.1"]
                 [jaxen/jaxen "1.1.6"]]
  :main swetc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
