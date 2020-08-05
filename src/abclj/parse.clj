(ns abclj.parse
  (:require [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [abclj.note :as note]))


(def current-key (atom {:key {}
                        :accidentals {}}))

(def default-duration (atom 1/8))

(defn- pitch-name
  [raw-pitch]
  (clojure.string/upper-case raw-pitch))

(defn- parse-pitch
  [raw-pitch]
  (let [name (pitch-name raw-pitch)]
    (+ (or (get-in @current-key [:accidentals name])
           (get-in @current-key [:key         name]))
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
            raw-pitch))))

(defn accidentals-for ;;FIXME
  [key]
  (let [sharp-count (.indexOf ["C" "D" "E" "F" "G" "A" "B"] key)]
    (merge {"A" 0
            "B" 0
            "C" 0
            "D" 0
            "E" 0
            "F" 0
            "G" 0}
           (into {} (take sharp-count
                          [["F" 1]
                           ["C" 1]
                           ["G" 1]
                           ["D" 1]
                           ["A" 1]
                           ["E" 1]
                           ["B" 1]])))))
(defn key-header
  [key]
  (swap! current-key assoc :key (accidentals-for key))
  [:K key])

(defn length-header
  ([numerator]
   (length-header numerator "1"))
  ([numerator denominator]
   (let [duration (/ (Long/parseLong numerator)
                     (Long/parseLong denominator))]
     (reset! default-duration duration)
     [:L duration])))

(defn- duration
  [numerator denominator]
  (/ (* @default-duration numerator)
     denominator))

(defn- parse-single-duration
  [duration]
  (if (re-matches #"/+" duration)
    [1 (math/expt 2 (count duration))]
    [(Long/parseLong duration) 1]))

(defn note
  [& args]
  (let [raw-pitch (->> args
                       (drop-while (comp (partial not= :Pitch) first))
                       first
                       second)]
    (doall (reduce (fn [note-so-far arg]
                     (match arg
                            [:Pitch       _]      note-so-far ;; already taken care of by the seed
                            [:Octave-Down _]      (update-in note-so-far [1 :pitch] #(- % note/octave))
                            [:Octave-Up   _]      (update-in note-so-far [1 :pitch] #(+ % note/octave))
                            [:Flat]               (do (swap! current-key assoc-in [:accidentals (:pitch-name (second note-so-far))] -1)
                                                      (note/flatten note-so-far))
                            [:Natural]            (do (swap! current-key assoc-in [:accidentals (:pitch-name (second note-so-far))] 0)
                                                      (note/naturalize note-so-far))
                            [:Sharp]              (do (swap! current-key assoc-in [:accidentals (:pitch-name (second note-so-far))] 1)
                                                      (note/sharpen note-so-far))
                            [:Duration numerator "/" denominator] (assoc-in note-so-far [1 :duration] (duration (Long/parseLong numerator) (Long/parseLong denominator)))
                            [:Duration length]    (assoc-in note-so-far [1 :duration] (apply duration (parse-single-duration length)))))
                   [:note {:pitch      (parse-pitch raw-pitch)
                           :pitch-name (pitch-name raw-pitch)
                           :duration   @default-duration}]
                   args))))


(defn barline
  [type-of-barline]
  (swap! current-key assoc :accidentals {})
  [:barline type-of-barline])

(defn beam-break
  [& _]
  [:beam-break])

(defn note-separator
  [separator]
  separator)
