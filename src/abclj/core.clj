(ns abclj.core
  (:require [abclj.parse :as parse]
            [clojure.java.io :as io]
            [instaparse.core :as insta]
            [clojure.edn :as edn]))

(def raw-grammar (.getPath (io/resource "abc.ebnf")))

(def abc->hiccup
  (insta/parser raw-grammar))

(def transformation-map
  {;;Main
   :Tunebook list

   ;;Headers
   :Headers (fn [& headers] [:Headers (into {} headers)])
   :X-Header (fn [number] [:X (edn/read-string number)])
   :Length-Header parse/length-header
   :Key-Header parse/key-header
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

   ;;Note Separation
   :Note-Separator parse/note-separator
   :Barline parse/barline
   :Beam-Break parse/beam-break
   })

(defn- result-or-empty
  [parsed-object]
  (if (insta/failure? parsed-object)
    []
    parsed-object))

(defn parse
  [abc]
  (->> abc
       (insta/parse abc->hiccup)
       (insta/transform transformation-map)))

(defn parse-file
  [filename]
  (-> filename slurp parse))
