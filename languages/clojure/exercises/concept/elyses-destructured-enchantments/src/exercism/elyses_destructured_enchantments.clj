(ns exercism.elyses-destructured-enchantments)

(defn first-card
  "Returns the first card from deck."
  [deck]
  (let [[first] deck]
    first))

(defn second-card
  "Returns the second card from deck."
  [deck]
  (let [[_ second] deck]
    second))

(defn swap-top-two-cards
  "Returns the deck with first two items reversed."
  [deck]
  (let [[a b & rest] deck]
    (vec (conj rest a b))))

(let [[a b & rest] [10 4 3 7 8]]
  (vec (conj rest a b)))

(comment
  (first-card [3])
  (first-card [8 3 9 5])
  (second-card [10 4])
  (second-card [2 5 1 6])
  (second-card [])
  (second-card [8])
  (swap-top-two-cards [3 6])
  (swap-top-two-cards [10 4 3 7 8])
  )