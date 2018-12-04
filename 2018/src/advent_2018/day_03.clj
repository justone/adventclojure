(ns advent-2018.day-03
  (:require
    [clojure.pprint :as pprint]
    [clojure.set :as cs]
    [advent-2018.common :as c]
    ))

;; Common

(defn parse
  [entry]
  (let [[id l r w h] (map c/parse-int (rest (re-matches #"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$" entry)))]
    {:id id :l l :r r :w w :h h}))


#_(->> (c/data-seq "day_03")
       (map parse)
       (pprint/print-table))

;; Part 1

(defn ->coords
  [entry]
  (let [{:keys [l r w h]} entry]
    (for [col (range l (+ l w))
          row (range r (+ r h))]
      [col row])))

(defn part-1
  [dataset]
  (->> (c/data-seq dataset)
       (map parse)
       (map ->coords)
       (map frequencies)
       (apply merge-with +)
       (filter (fn [[_ c]] (> c 1)))
       count))


#_(part-1 "day_03")
#_(part-1 "day_03_sample")

;; Part 2

(defn collide?
  [e1 e2]
  (boolean (and
    (not= (:id e1) (:id e2))
    (seq (cs/intersection (:coords e1) (:coords e2))))))

(defn ->coords2
  [entry]
  {:id (:id entry) :coords (set (->coords entry))})

(defn part-2
  [dataset]
  (let [all-entries (->> (c/data-seq dataset)
                     (map parse)
                     (map ->coords2))]
    (loop [entries all-entries]
      (let [[entry & rest] entries
            results (doall (map (partial collide? entry) all-entries))]
        (if (every? false? results)
          (:id entry)
          (recur rest))))))


#_(part-2 "day_03")
#_(part-2 "day_03_sample")
