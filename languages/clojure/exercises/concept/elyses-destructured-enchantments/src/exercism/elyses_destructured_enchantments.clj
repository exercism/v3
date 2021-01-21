(ns exercism.elyses-destructured-enchantments)

(defn first-card
  "Returns the first card from deck."
  [deck]
  (let [[first] deck]
    first))

(comment
  (first-card [3])
  (first-card [8 3 9 5])
  )