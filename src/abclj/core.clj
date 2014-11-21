(ns abclj.core
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]))

(def raw-grammar (.getPath (io/resource "abc.ebnf")))

(def parser (insta/parser raw-grammar))
