(ns abclj.write
  (:require [abclj.note :as note]
            [abclj.parse :as parse]
            [clojure.string :as str]))

(def current-key (atom {:key {}
                        :accidentals {}}))

(def default-duration (atom 1/8))

(defmulti ->abc first)

(defmethod ->abc :Tune
  [[_ & tune]]
  (str/join "\n" (map ->abc tune)))

(defmethod ->abc :Headers
  [[_ headers]]
  (when-let [length (:L headers)]
    (reset! default-duration length))
  (swap! current-key assoc :key (parse/accidentals-for (or (:K headers) "C")))
  (str/join "\n" (for [[header value] headers]
                   (format "%s: %s" (nth (str header) 1) value))))

(defmethod ->abc :Voice
  [[_ & voice]]
  (apply str (map ->abc voice)))

(defn accidental->abc
  [accidental]
  (get {-2 "__"
        -1 "_"
        0 "="
        1 "^"
        2 "^^"}
       accidental ""))

(defn pitch-with-octave
  [name pitch]
  (let [[case-fn octave-modifier] (if (>= pitch 12)
                                    [str/lower-case "'"]
                                    [str/upper-case ","])]
    (apply str (case-fn name) (if (<= 0 pitch 23)
                                [""]
                                (repeat (Math/abs (int (/ pitch 12)))
                                        octave-modifier)))))

(defmethod ->abc :note
  [[_ {:keys [pitch pitch-name duration]} :as note]]
  (let [accidental (note/pitch-adjustment note)]
    (str
     (if (= accidental
            (get (merge (:key @current-key) (:accidentals @current-key))
                 pitch-name))
       ""
       (do
         (swap! current-key assoc-in [:accidentals pitch-name] accidental)
         (accidental->abc accidental)))
     (pitch-with-octave pitch-name pitch)
     (if (= duration @default-duration) "" (/ duration @default-duration))))) ;;FIXME

(defmethod ->abc :beam-break
  [_]
  " ")

(defmethod ->abc :barline
  [[_ type]]
  (swap! current-key assoc :accidentals {})
  (if (= type "|")
    " | "
    type))
