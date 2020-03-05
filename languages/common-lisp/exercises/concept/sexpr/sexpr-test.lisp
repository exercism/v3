(in-package :sexpr-test)

(def-suite sexpr-suite)
(in-suite sexpr-suite)

(test atoms
  (is-true (sexpr:is-an-atom-p 5))
  (is-true (sexpr:is-an-atom-p 5.5))
  (is-true (sexpr:is-an-atom-p 'a))
  (is-true (sexpr:is-an-atom-p "hello"))
  (is-true (sexpr:is-an-atom-p t))
  (is-true (sexpr:is-an-atom-p :a-keyword))
  (is-true (sexpr:is-an-atom-p #'atom))
  (is-true (sexpr:is-an-atom-p #'consp))
  (is-true (sexpr:is-an-atom-p nil))

  (is-false (sexpr:is-an-atom-p '(1 2 3)))
  (is-false (sexpr:is-an-atom-p '(a . b)))
  (is-false (sexpr:is-an-atom-p (list 1 2 3)))
  (is-false (sexpr:is-an-atom-p (cons 'a 'b))))

(test conses
  (is-true (sexpr:is-a-cons-p '(a . b)))
  (is-true (sexpr:is-a-cons-p (cons 'a 'b)))
  (is-true (sexpr:is-a-cons-p (list 1 2 3)))

  (is-false (sexpr:is-a-cons-p 5))
  (is-false (sexpr:is-a-cons-p 5.5))
  (is-false (sexpr:is-a-cons-p 'a))
  (is-false (sexpr:is-a-cons-p "hello"))
  (is-false (sexpr:is-a-cons-p t))
  (is-false (sexpr:is-a-cons-p :a-keyword))
  (is-false (sexpr:is-a-cons-p #'atom))
  (is-false (sexpr:is-a-cons-p #'consp))
  (is-false (sexpr:is-a-cons-p nil)))
