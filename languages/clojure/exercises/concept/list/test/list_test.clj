(ns list-test
  (:require [clojure.test :refer [deftest is]]
            list))

(deftest list-new-test
  (is (= '() (list/new))))

(deftest list-add-test
  (is (= '() (list/add))))

(deftest list-remove-test
  (is (= '() (list/remove))))

(deftest list-query-test
  (is (= '() (list/query))))

(deftest list-count-test
  (is (= '() (list/count))))
