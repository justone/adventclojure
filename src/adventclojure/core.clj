(ns adventclojure.core
  (:gen-class)
  (:require [clojure.string :as st]))

; common functions --------- ; {{{

(defn filter-file [file fn]
  (with-open [reader (clojure.java.io/reader file)]
    (count (filter fn (line-seq reader)))))

(defn each-line [file fn init]
  (with-open [reader (clojure.java.io/reader file)]
    (reduce fn init (into [] (line-seq reader)))))

; }}}

; day 01 --------- ; {{{
(defn inc-dec [acc ch]
  ((if (= ch \() inc dec) acc))

(defn day-01-part-01 [file]
  (let [data (st/trim (slurp file))]
    (reduce inc-dec 0 data)))

(defn day-01-part-02 [file]
  (let [data (apply vector (st/trim (slurp file)))]
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

; day 03 --------- ; {{{
(defn move [[x y] dir]
  (condp = dir
    \> [(inc x) y]
    \< [(dec x) y]
    \^ [x (inc y)]
    \v [x (dec y)]
    ))

#_(move [0 0] \>)
#_(move [0 0] \<)
#_(move [0 0] \^)
#_(move [0 0] \v)

(defn locations [directions]
  (reduce #(conj %1 (move (last %1) %2)) [[0 0]] directions))

(defn locations-w-robot [directions]
  (->>
    (apply vector directions)
    (reduce-kv (fn [a i d] (update-in a [(rem i 2)] #(conj % (move (last %) d)))) [[[0 0]] [[0 0]]])
    (apply concat)))

#_(locations ">><<")
#_(locations "^v^v^v^v^v")
#_(locations-w-robot "<><>")
#_(locations-w-robot "^v^v^v^v^v")

(defn houses [file]
  (let [data (st/trim (slurp file))
        locations (locations data)]
    (count (set locations))))

(defn houses-w-robot [file]
  (let [data (st/trim (slurp file))
        locations (locations-w-robot data)]
    (count (set locations))))

#_(= 2572 (houses "day_03.txt"))
#_(= 2 (houses "day_03-2.txt"))
#_(= 2631 (houses-w-robot "day_03.txt"))
#_(= 11 (houses-w-robot "day_03-2.txt"))

; }}}

; day 04 --------- ; {{{

; from: http://rosettacode.org/wiki/MD5#Clojure
(defn md5 [in]
  (apply str
         (map (partial format "%02x")
              (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                         .reset
                         (.update (.getBytes in)))))))

#_(md5 "abcdef609043")
#_(md5 "pqrstuv1048970")

(defn find-first-zeroes [secret-key zeroes]
  (let [num-and-hash (juxt identity #(md5 (str secret-key %)))]
    (->> (iterate inc 1)
         (map num-and-hash)
         (filter #(.startsWith (second %) zeroes))
         (take 1)
         (apply first))))

#_(take 5 (iterate inc 1))
#_(= 282749 (find-first-zeroes "yzbqklnj" "00000"))
#_(= 9962624 (find-first-zeroes "yzbqklnj" "000000"))

; }}}

; day 05 --------- ; {{{

(defn three-vowels? [st]
  (boolean (re-find #"[aeiou].*[aeiou].*[aeiou]" st)))
(defn double-letters? [st]
  (boolean (re-find #"(.)\1" st)))
(defn no-bad-strs? [st]
  (boolean (re-find #"^((?!(ab|cd|pq|xy)).)*$" st)))

#_(three-vowels? "xazegov")
#_(three-vowels? "dvszwmarrgswjxmb")
#_(double-letters? "jchzalrnumimnmhp")
#_(double-letters? "ugknbfddgicrmopn")
#_(no-bad-strs? "haegwjzuvuyypxyu")
#_(no-bad-strs? "ugknbfddgicrmopn")

(def nice-string? (every-pred three-vowels? double-letters? no-bad-strs?))

#_(nice-string? "haegwjzuvuyypxyu")
#_(nice-string? "ugknbfddgicrmopn")
#_(filter-file "day_05-2.txt" nice-string?)
#_(= 258 (filter-file "day_05.txt" nice-string?))

(defn two-pairs? [st]
  (boolean (re-find #"(..).*\1" st)))
(defn every-other? [st]
  (boolean (re-find #"(.).\1" st)))

#_(two-pairs? "xyxy")
#_(two-pairs? "aaa")
#_(every-other? "xyxy")
#_(every-other? "aaa")

(def nice-string2? (every-pred two-pairs? every-other?))

#_(filter-file "day_05-2.txt" nice-string2?)
#_(= 53 (filter-file "day_05.txt" nice-string2?))

; }}}

; day 06 --------- ; {{{

(defn parse-instruction [inst-str]
  (let [[_ act & coords] (re-matches #"^([\w ]+) (\d+),(\d+) through (\d+),(\d+)$" inst-str)
        int-coords (map #(Integer/parseInt %) coords)]
    (conj int-coords (keyword (st/replace act #"\W" "-")))))

#_(parse-instruction "toggle 461,550 through 564,900")
#_(parse-instruction "turn off 370,39 through 425,839")
#_(parse-instruction "turn on 599,989 through 806,993")

(defn make-grid [len]
  (into [] (repeat len (into [] (repeat len 0)))))

#_(update-in (make-grid 6) [4 3] (:toggle actions))

(def actions-part-1
  {:toggle #(get [1 0] %)
   :turn-on (constantly 1)
   :turn-off (constantly 0)})

(def actions-part-2
  {:toggle (comp inc inc)
   :turn-on inc
   :turn-off #(let [n (dec %)] (if (> 0 n) 0 n))})

(defn print-count [f] (println (reduce + (apply concat f))) f)

; put (println instruction) in and wrap reduce with print-count to see steps
(defn act [actions grid instruction]
  (let [[action x1 y1 x2 y2] (parse-instruction instruction)
        action (action actions)]
    (reduce
      #(update-in %1 %2 action)
      grid
      (reverse (for [x (range x1 (inc x2)) y (range y1 (inc y2))] [x y])))))

#_(act "toggle 0,1 through 0,1" (make-grid 3))
#_(act "toggle 0,1 through 0,1" (act "turn on 1,2 through 1,2" (make-grid 3)))
#_(reduce act (make-grid 3) ["turn on 1,2 through 1,2" "toggle 0,1 through 0,1"])

(defn brightness [actions instruction-file grid-size]
  (->> (make-grid grid-size)
       (each-line instruction-file (partial act actions))
       (apply concat)
       (reduce +)))

#_(brightness actions-part-1 "day_06-2.txt" 4)
#_(= 543903 (brightness actions-part-1 "day_06.txt" 1000))
#_(brightness actions-part-2 "day_06-2.txt" 4)
#_(= 14687245 (brightness actions-part-2 "day_06.txt" 1000))

; }}}

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
