(ns advent-2018.day-04
  (:require
    [advent-2018.common :as c]
    ))


(defn match-regex
  [regex entry]
  (some->> (re-matches regex entry)
           rest))

(defn parse-begin-shift
  [entry]
  (if-let [[guard] (seq (map c/parse-int (match-regex #"^\[1518-\d+-\d+ \d+:\d+\] Guard #(\d+) begins shift$" entry)))]
    {:kind :begin-shift :guard guard}))

(defn parse-fall-asleep
  [entry]
  (if-let [[mins] (seq (map c/parse-int (match-regex #"^\[1518-\d+-\d+ \d+:(\d+)\] falls asleep$" entry)))]
    {:kind :falls-asleep :mins mins}))

(defn parse-wake-up
  [entry]
  (if-let [[mins] (seq (map c/parse-int (match-regex #"^\[1518-\d+-\d+ \d+:(\d+)\] wakes up$" entry)))]
    {:kind :wakes-up :mins mins}))

(defn parse
  [entry]
  (or
    (parse-begin-shift entry)
    (parse-fall-asleep entry)
    (parse-wake-up entry)))

(defn sorted-data
  [dataset]
  (sort (c/data-seq dataset)))

(defn parsed-data
  [dataset]
  (map parse (sort (c/data-seq dataset))))


#_(parsed-data "day_04")
#_(parsed-data "day_04_sample")

;; Part 1

(defn accept-entry
  [state entry]
  (let [{:keys [kind guard mins]} entry]
    (let [{:keys [current start]} state]
      (case kind
        :begin-shift (assoc state :current guard)
        :wakes-up (update-in state [:spans current] (fnil conj []) [start mins])
        :falls-asleep (assoc state :start mins)))))

(defn span-total
  [[guard spans]]
  (apply + (map (fn [[start end]] (- end start)) spans)))

;; frequencies are tuples, so these provide a better logical name
(def freq-minute first)
(def freq-count second)

(defn pick-highest-minute
  [freqs]
  (last (sort-by freq-count freqs)))

(defn span->freqs
  [span]
  (let [[start end] span]
    (frequencies (range start end))))

(defn spans->highest-minute
  [spans]
  (->> (map span->freqs spans)
       (apply merge-with +)
       pick-highest-minute))

(defn part-1
  [dataset]
  (let [guard-spans (:spans (reduce accept-entry {} (parsed-data dataset)))
        [guard spans] (last (sort-by span-total guard-spans))]
    (* guard (freq-minute (spans->highest-minute spans)))))


#_(part-1 "day_04")
#_(part-1 "day_04_sample")

;; Part 2

(defn max-span-minute-count
  [[guard spans]]
  (freq-count (spans->highest-minute spans)))

(defn part-2
  [dataset]
  (let [guard-spans (:spans (reduce accept-entry {} (parsed-data dataset)))
        [guard spans] (last (sort-by max-span-minute-count guard-spans))]
    (* guard (freq-minute (spans->highest-minute spans)))))


#_(part-2 "day_04")
#_(part-2 "day_04_sample")
