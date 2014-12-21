(ns abclj.core
  (:require [abclj.parsing :refer [hiccup->edn]]
            [abclj.parse :as parse]
            [clojure.java.io :as io]
            [instaparse.core :as insta]
            [clojure.edn :as edn]))

(comment test
  (clojure.pprint/pprint
   (insta/transform transformation-map
                    (insta/parse abc->hiccup
                                 (slurp "tmp/sample.abc"))))
)


(def raw-grammar (.getPath (io/resource "abc.ebnf")))

(def abc->hiccup
  (insta/parser raw-grammar))

(def transformation-map
  {;;Main
   :Tunebook list

   ;;Headers
   :Headers (fn [& headers] [:Headers (into {} headers)])
   :X-Header (fn [number] [:X (edn/read-string number)])
   :Key-Header (fn [key] [:K key])
   :Key (fn [raw-key] raw-key)
 ;  :Key-Accidental identity
  ; :Mode identity
   :Generic-Header (fn [name value] [(keyword (str (first name))) value])
;   :Voice-Header identity

   ;;Tune Body
;   :Voice identity
;   :Note-Separator identity

   ;;Notes
   :Note parse/note
;   :Note-Prefix identity
;   :Pitch identity
;   :Note-Suffix identity
;   :Decoration identity
;   :Duration identity
;   :Octave-Modifier identity
;   :Octave-Down identity
;   :Octave-Up identity

   ;;Note Separation
;   :Barline identity
;   :Beam-Break identity

   ;;Miscellaneous
;   :Whitespace identity
;   :EOL identity
   })

(defn- result-or-empty
  [parsed-object]
  (if (insta/failure? parsed-object)
    []
    parsed-object))

(defn parse
  [abc]
  (letfn []
    (-> abc
        abc->hiccup
        result-or-empty
        hiccup->edn)))

(defn parse-file
  [filename]
  (-> filename slurp parse))
