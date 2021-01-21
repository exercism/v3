(ns exercism.elyses-destructured-enchantments-test
  (:require [clojure.test :refer :all]
            [exercism.elyses-destructured-enchantments :refer :all]))

(deftest first-card-test
  (is (= (first-card [3]) 3))
  (is (= (first-card [8 3 9 5]) 8)))
