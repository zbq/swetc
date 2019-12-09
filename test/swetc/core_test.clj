(ns swetc.core-test
  (:require [clojure.test :refer :all]
            [swetc.core :refer :all]))

(deftest cyclic-dep-test
  (testing "cyclic dependencies test"
    (is (= (find-cyclic-dependencies '{a #{b}, b #{c}, c #{a}})
           '([a b c])))
    (is (= (find-cyclic-dependencies '{a #{b c a}, b #{a}, c #{b}})
           '([a] [a c b] [a b])))))

(deftest proj->direct-dep-projs-test
  (testing "direct dependent projects of project"
    (let [proj->info {"TestX/A.vcxproj" {:TargetName "A" :Dependencies #{"B"}}
                      "TestY/B.vcxproj" {:TargetName "B" :Dependencies #{"C"}}
                      "TestZ/C.vcxproj" {:TargetName "C" :Dependencies #{"D"}}}
          target->proj (map-target->proj proj->info)
          proj->direct-dep-projs (map-proj->direct-dep-projs proj->info target->proj)]
      (is (= target->proj
             {"A" "TestX/A.vcxproj",
              "B" "TestY/B.vcxproj",
              "C" "TestZ/C.vcxproj"}))
      (is (= proj->direct-dep-projs
             {"TestX/A.vcxproj" (list "TestY/B.vcxproj")
              "TestY/B.vcxproj" (list "TestZ/C.vcxproj")
              "TestZ/C.vcxproj" ()})))))

(deftest xmlns-test
  (testing "xmlns & xpath"
    (let [xmlstr "
<root xmlns='http://github.com/zbq'>
  <hello id='1'>world1</hello>
  <hello id='2'>world2</hello>
</root>"
          xml-doc (xml-doc-from-string xmlstr)]
      (is (= (xmlns-of-node (.getRootElement xml-doc))
             "http://github.com/zbq"))
      (is (= (xpath-select-one xml-doc "/root/hello[@id='1']/text()")
             nil))
      (is (= (.getText (xpath-select-one xml-doc "/n:root/n:hello[@id='1']/text()" {"n" "http://github.com/zbq"}))
             "world1")))))

(deftest xml-quote-test
  (testing "xml quote"
    (let [xmlstr "
<root>
  <hello id='&quot;I&apos;m id&quot;'>world</hello>
</root>"
          xml-doc (xml-doc-from-string xmlstr)]
      (is (= (.getText (xpath-select-one xml-doc "/root/hello/@id"))
             "\"I'm id\"")))))

(deftest map-of-entries-test
  (testing "map-of-entries"
    (is (= (map-of-entries [(clojure.lang.MapEntry. 1 2)
                            (clojure.lang.MapEntry. 2 3)])
           {1 2, 2 3}))))

