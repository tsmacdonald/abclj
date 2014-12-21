(ns abclj.parse
  (:require [clojure.core.match :refer [match]]
            [abclj.note :as note]))



(defn- parse-pitch
  [raw-pitch]
  (get {"C" 0
        "D" 2
        "E" 4
        "F" 5
        "G" 7
        "A" 9
        "B" 11
        "c" 12
        "d" 14
        "e" 16
        "f" 17
        "g" 19
        "a" 21
        "b" 23}
       raw-pitch))

(defn note
  [& args]
  (let [raw-pitch (->> args
                       (drop-while (comp (partial not= :Pitch) first))
                       first
                       second)]
    (reduce (fn [note-so-far arg]
              (match arg
                [:Pitch       _] note-so-far ;; already taken care of by the seed
                [:Octave-Down _] (update-in note-so-far [:pitch] #(- % note/octave))
                [:Octave-Up   -] (update-in note-so-far [:pitch] #(+ % note/octave))
                [:Flat]          (note/flatten note-so-far)
                [:Natural]       (note/naturalize note-so-far)
                [:Sharp]         (note/sharpen note-so-far)
                )


              ) {:pitch (parse-pitch raw-pitch) :pitch-name (clojure.string/upper-case raw-pitch)}
                args))



  (prn args)
  args)
