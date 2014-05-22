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
