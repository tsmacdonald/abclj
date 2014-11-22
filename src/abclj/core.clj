(ns abclj.core
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(def raw-grammar (.getPath (io/resource "abc.ebnf")))

(def parser (insta/parser raw-grammar))

(defmacro parsing-error
  [location tag-name]
  (let [location# location
        tag-name# tag-name]
    `(throw (Exception. (str "Unexpected tag in " ~location ": " ~tag-name)))))

(defn parse-key
  [raw-key]
  ;;TODO modes etc
  {:tonic (second raw-key)})

(defn- parse-header
  ;;TODO this could be much fancier
  [name-with-colon value]
  [(keyword (str/upper-case (first name-with-colon)))
   (second value)])

(defn- header->edn
  [hiccup]
  (match hiccup
         [:X-Header "X:" n] [:number (Long/parseLong n)]
         [:Key-Header "K:" k] [:key (parse-key k)]
         [:Generic-Header k v] (parse-header k v)
         :else (parsing-error "headers" (first hiccup))))

(defn- voice->edn
  [hiccup]
  ;;TODO
  ":)")

(defn- tune-part->edn
  [tune-so-far hiccup]
  (match hiccup
         [:Headers & _] (assoc tune-so-far :headers (into {} (map header->edn (rest hiccup))))
         [:Voice & _] (assoc tune-so-far :voices (conj (or (:voices tune-so-far) [])
                                                       (map voice->edn (rest hiccup))))
         :else (parsing-error "tune" (first hiccup))))

(defn- tune->edn
  [hiccup]
  (reduce tune-part->edn {} hiccup))

(defn hiccup->edn
  [hiccup]
  (match hiccup
         [:Tunebook & _] (map hiccup->edn (rest hiccup))
         [:Tune & _] (tune->edn (rest hiccup))
    :else (parsing-error "toplevel" (first hiccup))))

(defn- result-or-empty
  [parsed-object]
  (if (insta/failure? parsed-object)
    []
    parsed-object))

(defn parse
  [abc-string]
  (-> abc-string
      parser
      result-or-empty
      hiccup->edn))

(defn parse-file
  [filename]
  (-> filename slurp parse))
