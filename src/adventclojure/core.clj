(ns adventclojure.core
  (:gen-class)
  (:require [clojure.string :refer [trim]]))

; day 01 --------- ; {{{
(defn inc-dec [acc ch]
  ((if (= ch \() inc dec) acc))

(defn day-01-part-01 [file]
  (let [data (trim (slurp file))]
    (reduce inc-dec 0 data)))

(defn day-01-part-02 [file]
  (let [data (apply vector (trim (slurp file)))]
    (reduce-kv (fn [acc idx ch] 
                 (let [new (inc-dec acc ch)]
                   (if (= -1 new) (reduced (inc idx)) new))) 0 data)))

#_(= 232 (day-01-part-01 "day_01.txt"))
#_(= -3 (day-01-part-01 "day_01-2.txt"))
#_(= 1783 (day-01-part-02 "day_01.txt"))
#_(= 5 (day-01-part-02 "day_01-2.txt"))

; }}}

; day 02 --------- ; {{{
(defn paper-per-box [dimensions]
  (let [sides (map read-string (re-seq #"\d+" dimensions))
        [h l w] sides
        paper (* 2 (+ (* h l) (* h w) (* l w)))
        extra (apply * (take 2 (sort sides)))]
    (+ paper extra)))

(defn ribbon-per-box [dimensions]
  (let [sides (map read-string (re-seq #"\d+" dimensions))
        perimiter (reduce * sides)
        bow (* 2 (apply + (take 2 (sort sides))))]
    (+ perimiter bow)))

(defn each-box [file fn]
  (with-open [reader (clojure.java.io/reader file)]
    (reduce + (map fn (line-seq reader)))))


#_(= 58 (paper-per-box "2x3x4"))
#_(= 43 (paper-per-box "1x1x10"))
#_(= 1606483 (each-box "day_02.txt" paper-per-box))
#_(= 101 (each-box "day_02-2.txt" paper-per-box))
#_(= 3842356 (each-box "day_02.txt" ribbon-per-box))
#_(= 48 (each-box "day_02-2.txt" ribbon-per-box))

; }}}

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
