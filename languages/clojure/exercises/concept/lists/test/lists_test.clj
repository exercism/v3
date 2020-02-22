(ns lists-test
  (:require [clojure.test :refer [deftest is]]
            lists))

(deftest list-empty-test
  (is (= '() (lists/empty-list)))
)

(deftest list-add-test
    (is (= '("JavaScript" "Java" "Lisp" "Clojure") 
            (lists/add-item "JavaScript" 
              (lists/add-item "Java"
                (lists/add-item "Lisp" 
                  (lists/add-item "Clojure" '())
                )
              )
            )
        )
    )
)

(deftest list-remove-test
  (is (= '("JavaScript" "Java" "Clojure") (lists/remove-item "Lisp" '("JavaScript" "Java" "Lisp" "Clojure")))))

(deftest list-query-test
  (is (= nil (lists/query-item "Lisp" '("JavaScript" "Java" "Clojure")))))

(deftest list-count-test
  (is (= 3 (lists/count-list '("JavaScript" "Java" "Clojure")))))

(deftest list-sort-test
  (is (= '("Clojure" "Java" "JavaScript") (lists/sort-list '("JavaScript" "Java" "Clojure")))))
