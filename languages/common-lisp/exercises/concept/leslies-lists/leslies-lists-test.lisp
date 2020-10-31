;; Ensures that leslies-lists.lisp and the testing library are always loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "leslies-lists")
  (ql:quickload :fiveam))

;; Defines the testing package with symbols from leslies-lists and FiveAM in scope
;; The `run-tests` function is exported for use by both the user and test-runner
(defpackage leslies-lists-test
  (:use :cl :fiveam :leslies-lists)
  (:export :run-tests))

;; Enter the testing package
(in-package :leslies-lists-test)

;; Define and enter a new FiveAM test-suite
(def-suite leslies-lists-suite)
(in-suite leslies-lists-suite)

(test new-list "Leslie needs a way to make a new empty list"
      (is (equal '() (new-list))))

(test list-of-things "Leslie needs to create a new list with three things on it"
      (is (equal '(salt skquargzes butter)
                 (list-of-things 'salt 'skquargzes 'butter))))

(test adding-to-the-list "Leslie needs a way of adding items to a list"
      (is (equal '(left-handed-frobz)
                 (add-to-list 'left-handed-frobz (new-list))))
      (is (equal '(left-handed-frobz butter)
                 (add-to-list 'left-handed-frobz '(butter)))))

(test peeking-at-the-list "Leslie needs a way to see what items are coming up on the list"
      (let ((shopping-list '(left-handed-frobz salt skquargzes butter sananab
                             motor-oil dilithium-crystals photonic-oscillators
                             digestive-biscuits marmalade jelly-babies
                             hazramfoobles crisps chips right-handed-macaroni
                             various-nozzles seedless-snozzberries ronopotolo
                             cran-apple apple-cran raisins dihydrogen-oxide
                             birthday-candles cupcakes)))
        (is (equal 'left-handed-frobz (first-thing shopping-list)))
        (is (equal 'salt (second-thing shopping-list)))
        (is (equal 'skquargzes (third-thing shopping-list)))
        (is (equal 'cupcakes (twenty-third-thing shopping-list)))))


(test removing-a-item "Leslie needs to see the list after removing the first item"
      (is (equal '(salt skquargzes butter sananab)
                 (remove-first-item
                  '(left-handed-frobz salt skquargzes butter sananab)))))

(test put-two-lists-together "Leslie needs to add one list to another"
      (is (equal '(left-handed-frobz salt skquargzes butter)
                 (list-append '(left-handed-frobz salt)
                              '(skquargzes butter)))))

(test how-long-is-the-list "Leslie wants to know how much shopping is left"
      (is (= 0 (just-how-long '())))
      (is (= 3 (just-how-long '(left-handed-frobz salt skquargzes))))
      (is (= 2 (just-how-long '(left-handed-frobz salt))))
      (is (= 11 (just-how-long '(left-handed-frobz salt skquargzes butter sananab
                                 motor-oil dilithium-crystals photonic-oscillators
                                 digestive-biscuits marmalade jelly-babies)))))

;; Either provides human-readable results to the user or machine-readable
;; results to the test runner. The default upon calling `(run-tests)` is to
;; explain the results in a human-readable way
(defun run-tests (&optional (explain t))
  (let ((tests (run 'leslies-lists-suite))) ; Run the tests once
    (if explain (explain! tests) tests))) ; Optionally explain the results
