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

  (testing "palindrome in general"
    (is (= true (#(if (string? %) (= (clojure.string/reverse %) %) (= (reverse %) %)) "paap"))  )
    (is (= true (#(if (string? %) (= (clojure.string/reverse %) %) (= (reverse %) %)) '(1 2 2 1)))  )
    (is (= true (#(if (string? %) (= (clojure.string/reverse %) %) (= (reverse %) %)) '(1 2 4 2 1)))  )
    (is (= false (#(if (string? %) (= (clojure.string/reverse %) %) (= (reverse %) %))  [1 2 3 4 2 1]))  )
    (is (= false (#(= (reverse %) (seq %))  [1 2 3 4 2 1]))  )

  )

  (testing "remove duplicates in strings"
    (is (= (apply str ((fn my_accum [inp] (loop [xs inp
                                                 result []]
                                            (if xs
                                              (let [x (first xs)]
                                                (if (= (last result) x) (recur (next xs) result) (recur (next xs) (conj result x)) ))
                                              result)))  "Leeeerrroyyyyyyy")) "Leroy" )
    )
  )

  (testing "remove duplicates in strings - most elegant way"
    (is (= (apply str (#(map first (partition-by identity %)) "Leeeeooorrroyyyyyyy")) "Leoroy" )
        )
    )

  (testing "drop every third element in array"
    (is (= (#(flatten (partition (- %2 1) %2 nil %1)) [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
    )

  (testing "drop every fourth element in array"
    (is (= (#(flatten  (partition (- %2 1) %2 nil %1)) [1 2 3 4 5 6] 4) [1 2 3 5 6]))
    )

  (testing "drop every fourth character in string"
    (is (= (#(apply str (flatten  (partition (- %2 1) %2 nil %1))) "tikitakatekutoko" 4) "tiktaktektok"))
    )

)

(deftest day5
  (testing "repeat n times any array element"
    (is (= (#(reduce into [] (map (fn [x] (repeat %2 x)) %1)) [1 2 3] 4) [1 1 1 1 2 2 2 2 3 3 3 3]))
    )

  (testing "repeat x times any array in a list of arrays"
    (is (= (#(reduce into [] (map (fn [x] (repeat %2 x)) %1)) [[1 2] [2 3] [3 4]] 2) '([1 2] [1 2] [2 3] [2 3] [3 4] [3 4]) ))
    )

  (testing "repeat x times any symbol in an array"
    (is (= (#(reduce into [] (map (fn [x] (repeat %2 x)) %1)) [:a :b] 4) '(:a :a :a :a :b :b :b :b)) )
    )

  (testing "repeat x times with a very compact method"
    (is (= ((fn [xs n] (mapcat #(repeat n %) xs)) [44 33] 2) [44 44 33 33])) )

)

(deftest fibonacci_numbers
  (testing "writes a functions that returns the first n fibonacci numbers"
    (is (= ((fn fibos [n] (
                            case n
                            1 [1]
                            2 [1 1]
                            (conj (fibos (- n 1)) (reduce + (take-last 2 (fibos (- n 1)))))
                            )
              ) 7) [1 1 2 3 5 8 13]))
    )

  (testing "writes a functions using iterate that returns the first n fibonacci numbers"
    (is (= (take 7 (map first (iterate (fn [[a b]] [b (+' a b)]) [0 1]))) [0 1 1 2 3 5 8]))
    )
  )

(deftest get_caps
  (testing "writes a functions that returns only caps in a string"
    (is (= (#(clojure.string/join "" (re-seq #"[A-Z]" %)) "HeLlO, WoRlD!") "HLOWRD"))
    )
  )

(deftest intro_some
  (testing "learn how some is working"
    (is (= (some #{2 7 6} [5 6 7 8]) 6))
    )
  )

(deftest fact
  (testing "a function that calculates factorial"
    (is (= ((fn fact [x] (case x
                           1 1
                           (* x (fact (- x 1))))) 5) 120))
  ))

(deftest destruct
  (testing "intro to destruct"
    (is (= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e])))
    )
  )

(deftest half-truth
  (testing "returns true if some is true but not all are true - case 1"
    (is (=  ((fn [first & rest] (or (and (not first) (some true? rest)) (and first (not (every? true? rest))) )) true true true false) true)))
  (testing "returns true if some is true but not all are true - case 2"
    (is (=  ((fn [first & rest] (or (and (not first) (some true? rest)) (and first (not (every? true? rest))) )) true false false) true)))
  (testing "returns true if some is true but not all are true - case 3"
    (is (=  ((fn [first & rest] (or (and (not first) (some true? rest)) (and first (not (every? true? rest))) )) false true true false) true)))
  (testing "returns true if some is true but not all are true - case 4"
    (is (=  ((fn [first & rest] (or (and (not first) (some true? rest)) (and first (not (every? true? rest))) )) false false false true false) true)))
  (testing "returns false if all false"
    (is (=  ((fn [first & rest] (or (and (not first) (some true? rest)) (and first (not (every? true? rest))) )) false false false) false)))
  (testing "returns false if all true"
    (is (=  ((fn [first & rest] (or (and (not first) (some true? rest)) (and first (not (every? true? rest))) )) true true true true) false)))
  )

(deftest gcd
  (testing "greates common divisor of 1023 and 858 is 33"
    (is (= 33 ((fn gcd [a b] (loop [x a
                                     y b]
                                (if (= x y)
                                  x
                                  (let [xmin (min x y)
                                        xmax (max x y)]

                                  (recur (- xmax xmin) xmin)
                                  )
                                ))) 1023 858)))
    )
  (testing "greates common divisor of 1023 and 858 is 33 - shorter version"
    (is (= 33 ((fn gcd [a b] (cond
                               (= a b) a
                               (> a b) (recur (- a b) b)
                               :else (recur (- b a) a)
                                 )) 1023 858)))
    )
  (testing "greates common divisor of 1023 and 858 is 33 - much faster version - look at remainders"
    (is (= 33 ((fn gcd [a b] (if (zero? b) a (recur b (rem a b)))) 1023 858)))
    )
  )


