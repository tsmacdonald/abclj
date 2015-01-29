(ns abclj.tune)

(defn headers
  [tune]
  (second tune))

(defn voices
  [tune]
  (nth tune 2))
