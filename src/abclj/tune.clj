(ns abclj.tune)

(defn of-type
  [type-name element]
  (and (coll? element)
       (= (first element) type-name)))

(defn headers
  [tune]
  (second tune))

(defn voices
  [tune]
  (filter (partial of-type :Voice) tune))

(def melody (comp first voices))

(defn notes
  [voice]
  (filter (partial of-type :note) voice))
