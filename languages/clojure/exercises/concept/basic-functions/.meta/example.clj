(ns basic-functions)

(defn new-function
  "Creates a function that returns the answer to the ultimate question of life, the universe, and everything."
  []
  42)

(defn plus-42
  "Creates a function that adds 42 to the argument its given."
  [x]
  (+ x 42))

(defn invoke-plus-42
  "Calls the plus-42 function with the supplied argument"
  [y]
  (plus-42 y))

