;;; instead of the example code here is a sketch of the test code with a stub implementation:
(define-condition explosion () ())
(define-condition sad-trombone () ())
(define-condition victory ())

(defconstant +mazes+ '((maze-1 '((("wrong" . "WRONG") . explosion)
                                 ((2 2.0) . explosion)
                                 ((lisp LISP)) . victory))
                       (maze-2 '(((#\a #\A) . explosion)
                                 ((#\a #\a) . victory)))))

(defun run-maze (maze-id robot)
  (let ((key (robot maze-id))
        (maze (assoc maze-id +mazes+)))
    (loop
       for (door . behind-the-door) in maze
       do (when (apply key door) (signal behind-the-door))
       finally (signal 'sad-trombone))))

(define-test maze-1-object-equality
  (signals 'victory (run-maze 'maze-1 #'robot)))

;; ==================== Implementation ====================

(defun robot (maze-id)
  (case maze-id
    ;; add forms such as:
    ;; (maze-id-13 #'equalp)

    (t (constantly t))))
