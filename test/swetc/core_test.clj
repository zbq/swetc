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
             {"TestX/A.vcxproj" #{"TestY/B.vcxproj"}
              "TestY/B.vcxproj" #{"TestZ/C.vcxproj"}
              "TestZ/C.vcxproj" #{}})))))

