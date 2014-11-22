(ns abclj.core
  (:require [abclj.parsing :refer [hiccup->edn]]
            [clojure.java.io :as io]
            [instaparse.core :as insta]))

(def raw-grammar (.getPath (io/resource "abc.ebnf")))

(def abc->hiccup
  (insta/parser raw-grammar))

(defn parse
  [abc]
  (letfn [(result-or-empty [parsed-object]
            (if (insta/failure? parsed-object)
              []
              parsed-object))]
    (-> abc
        abc->hiccup
        result-or-empty
        hiccup->edn)))

(defn parse-file
  [filename]
  (-> filename slurp parse))
