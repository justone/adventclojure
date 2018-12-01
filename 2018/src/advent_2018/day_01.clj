(ns advent-2018.day-01
  (:require
    [advent-2018.common :as c]
    [clojure.string :as string]
    ))

(defn data-ints
  [dataset]
  (mapv c/parse-int (c/data-seq dataset)))

;; Part 1
(defn part-1
  [dataset]
  (apply + (data-ints dataset)))

#_(part-1 "day_01")
#_(part-1 "day_01_sample")

;; Part 2

(defn next-valid-idx
  [changes idx]
  (let [next-idx (inc idx)]
    (if (get changes next-idx)
      next-idx
      0)))

(defn part-2
  [dataset]
  (let [changes (data-ints dataset)]
    (loop [visited #{0}
           current 0
           idx 0]
      (let [new-val (+ current (get changes idx))]
        (if (visited new-val)
          new-val
          (recur (conj visited new-val)
                 new-val
                 (next-valid-idx changes idx)))))))

#_(part-2 "day_01")
#_(part-2 "day_01_sample")

;; something with cycle could work...
(take 10 (cycle (data-ints "day_01_sample")))
