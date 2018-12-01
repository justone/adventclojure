(ns advent-2018.common
  (:require
    [clojure.java.io :as io]
    ))

(defn data
  "Return a given dataset resource."
  [dataset]
  (io/resource (str dataset ".txt")))

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn data-seq
  "Return a sequence of lines for a given dataset."
  [dataset]
  (line-seq (io/reader (data dataset))))

