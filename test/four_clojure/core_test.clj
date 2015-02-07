(ns four-clojure.core-test
  (:require [clojure.test :refer :all]
            [four-clojure.core :refer :all]
            [midje.sweet :refer :all]))

(facts "Problem 86"
  (fact "7 is a happy number"
    (happy-numbers 7) => true)
  (fact "986543210 is a happy number"
    (happy-numbers 986543210) => true)
  (fact "2 is an unhappy number"
    (happy-numbers 2) => false)
  (fact "3 is an unhappy number"
    (happy-numbers 3) => false))

(facts "Problem 150"
       (fact "It can return the first 26 palendromic numbers"
             (= (take 26 (palendromic 0))
                [0 1 2 3 4 5 6 7 8 9 
                 11 22 33 44 55 66 77 88 99 
                 101 111 121 131 141 151 161]) => true))
