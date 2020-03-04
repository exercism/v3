(defsystem "sexpr"
  :name "sexpr"
  :version "0.0.0"
  :description "Exercism Common Lisp concept exercise for s-expressions"
  :serial t
  :components ((:file "package")
               (:file "sexpr"))
  :in-order-to ((test-op (test-op "sexpr/test"))))

(defsystem "sexpr/test"
  :name "sexpr/test"
  :version "0.0.0"
  :description "Tests for the 'sexpr' concept exercise"
  :depends-on ("sexpr" "fiveam")
  :serial t
  :components ((:file "sexpr-test"))

  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call :fiveam '#:run! 'sexpr-test:sexpr-suite)))
