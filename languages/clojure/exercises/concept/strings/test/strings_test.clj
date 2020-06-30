(ns strings-test
  (:require [clojure.test :refer [deftest testing is]]
            strings))

(deftest error-message-test
  (is (= "Stack overflow" (strings/message "[ERROR]: Stack overflow"))))

(deftest warning-message-test
  (is (= (strings/message "[WARNING]: Disk almost full") "Disk almost full")))

(deftest info-message-test
  (is (= (strings/message "[INFO]: File moved") "File moved")))

(deftest message-trim-test
  (is (= "Timezone not set" (strings/message "[WARNING]:   \tTimezone not set  \r\n"))))

(deftest error-log-level-test
  (is (= "error" (strings/log-level "[ERROR]: Disk full"))))

(deftest warning-log-level-test
  (is (= "warning" (strings/log-level "[WARNING]: Unsafe password"))))

(deftest info-log-level-test
  (is (= "info" (strings/log-level "[INFO]: Timezone changed"))))

(deftest error-reformat-test
  (is (= "Segmentation fault (error)" (strings/reformat "[ERROR]: Segmentation fault"))))

(deftest warning-reformat-test
  (is (= "Decreased performance (warning)" (strings/reformat "[WARNING]: Decreased performance"))))

(deftest info-reformat-test
  (is (= "Disk defragmented (info)" (strings/reformat "[INFO]: Disk defragmented"))))

(deftest reformat-trim-test
  (is (= "Corrupt disk (error)" (strings/reformat "[ERROR]: \t Corrupt disk\t \t \r\n"))))
