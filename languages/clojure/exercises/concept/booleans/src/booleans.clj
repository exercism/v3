(ns booleans)

(defn can-fast-attack? [knight-awake?]
  (not knight-awake?))

(defn can-spy? [knight? archer? prisoner?]
  (or knight? archer? prisoner?))

(defn can-signal-prisoner? [archer? prisoner?]
  (and (not archer?)
       prisoner?))

(defn can-free-prisoner? [knight? archer? prisoner? dog?]
  (or (and (not knight?)
           (not archer?)
           prisoner?)
      (and (not archer?)
           dog?)))