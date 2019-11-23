(ns swetc.core-test
  (:require [clojure.test :refer :all]
            [swetc.core :refer :all]))

(deftest cyclic-dep-test
  (testing "cyclic dependencies test"
    (is (= (find-cyclic-dependencies '{a #{b}, b #{c}, c #{a}})
           '([a b c])))
    (is (= (find-cyclic-dependencies '{a #{b c a}, b #{a}, c #{b}})
           '([a] [a c b] [a b])))))

