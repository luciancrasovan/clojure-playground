(ns serpent-talk.core-test
  (:require [clojure.test :refer :all]
            [serpent-talk.core :refer :all]))

(deftest a-test
  (testing "Simple recursion"
    (is (= nil ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 0)))
    (is (= '(2 1) ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 2)))
    (is (= '(5 4 3 2 1) ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5)))
    )

  (testing "thread-like stuff"
    (is (= (reduce + 0 (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
           (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce + 0))
           11))
    )

  (testing "for the win"
    (is (= [1 5 9 13 17 21 25 29 33 37] (for [x (range 40)
                    :when (= 1 (rem x 4))]
                x))))

  (testing "palindrome"
    (is (= true (#(= (clojure.string/reverse %) %) "paap"))  )
  )

  )