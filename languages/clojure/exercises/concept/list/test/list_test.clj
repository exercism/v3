(ns language-list-test
  (:require [clojure.test :refer [deftest is]]
            list))

(deftest list-new-test
  (is (= '() (list/new-item))))

(deftest list-add-test
  (is (= '() (list/add-item))))

(deftest list-remove-test
  (is (= '() (list/remove-item))))

(deftest list-query-test
  (is (= '() (list/query-item))))

(deftest list-count-test
  (is (= '() (list/count-list))))
