(ns serpent-talk.core-test
  (:require [clojure.test :refer :all]
            [serpent-talk.core :refer :all]))

(deftest a-test
  (testing "Simple recursion"
    (is (= nil ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 0)))
    (is (= '(2 1) ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 2)))
    (is (= '(5 4 3 2 1) ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5)))
    ))

