(ns abclj.note
  (:refer-clojure :exclude [flatten])
  (:require [plumbing.core :refer :all]))

(def octave 12)

(def name->pitch {"C" 0
                  "D" 2
                  "E" 4
                  "F" 5
                  "G" 7
                  "A" 9
                  "B" 11})

(defn relativize
  [pitch]
  (mod pitch octave))

(defn flat?
  [{:keys [pitch pitch-name]}]
  (= (get (map-vals (comp relativize dec) name->pitch) pitch-name)
     (relativize pitch)))

(defn natural?
  [{:keys [pitch pitch-name]}]
  (= (get name->pitch pitch-name)
     (relativize pitch)))

(defn sharp?
  [{:keys [pitch pitch-name]}]
  (= (get (map-vals (comp relativize inc) name->pitch) pitch-name)
     (relativize pitch)))

(defn flatten
  [note]
  (cond
    (flat?    note) note
    (natural? note) (update-in note [:pitch] dec)
    (sharp?   note) (update-in note [:pitch] (comp dec dec))))

(defn naturalize
  [note]
  (cond
    (flat?    note) (update-in note [:pitch] inc)
    (natural? note) note
    (sharp?   note) (update-in note [:pitch] dec)))

(defn sharpen
  [note]
  (cond
    (flat?    note) (update-in note [:pitch] (comp inc inc))
    (natural? note) (update-in note [:pitch] inc)
    (sharp?   note) note))
