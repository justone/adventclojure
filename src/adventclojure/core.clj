(ns adventclojure.core
  (:gen-class)
  (:require [clojure.string :refer [trim]]))

; day 01 ---------
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

#_(day-01-part-01 "day_01.txt")
#_(day-01-part-01 "day_01-2.txt")
#_(day-01-part-02 "day_01.txt")
#_(day-01-part-02 "day_01-2.txt")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
