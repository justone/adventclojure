(ns advent-2018.day-02
  (:require
    [advent-2018.common :as c]
    ))

;; Part 1

(defn has?
  [number]
  (fn [stat] (some #(= number %) (vals stat))))

(defn part-1
  [dataset]
  (let [stats (map frequencies (c/data-seq dataset))]
    (* (count (filter (has? 2) stats))
       (count (filter (has? 3) stats)))))


#_(part-1 "day_02")
#_(part-1 "day_02_sample")

;; Part 2

(defn common-letters
  [from to]
  (->> (interleave from to)
       (partition 2)
       (filter #(apply = %))
       (map first)
       (apply str)))

(defn differ-by-one?
  [from to]
  (let [common (common-letters from to)]
    (if (= (dec (count from)) (count common))
      common)))

(defn part-2
  [dataset]
  (loop [data (c/data-seq dataset)]
    (let [[from & the-rest] data]
      (if-let [common (first (filter some? (map (partial differ-by-one? from) the-rest)))]
        common
        (recur the-rest)))))


#_(differ-by-one? "abb" "abd")
#_(differ-by-one? "abb" "bbd")
#_(part-2 "day_02")
#_(part-2 "day_02_sample2")
