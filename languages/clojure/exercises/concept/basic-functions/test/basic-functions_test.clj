(ns basic-functions-test
  (:require [clojure.test :refer [deftest is]]
            basic-functions))

(deftest new-function-returns-42
  (is (= 42 (new-function))))

(deftest plus-42-adds-42
  (is (= 0 (plus-42 42)

(deftest invoke-plus-42-adds-42
  (is (= 0 (invoke-plus-42 42)

