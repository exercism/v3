(ns concept-list-test
  (:require [clojure.test :refer [deftest is]]
            concept-list))

(deftest concept-list-test
  (is (= "Hello, World!" (hello-world/hello))))
