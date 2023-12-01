(ns aoc23.day1
  (:require [clojure.string :as str])
  (:import re))

(def lines (-> (slurp "aoc23/day1.input")
               str/split-lines))

;; Part 1
(let [nums (map #(let [nums (filter (fn [c] (.isdigit c)) %)]
                   (int (str (first nums) (last nums))))
                lines)]
  (reduce + nums))
;; => 54605

;; Part 2
(def numbers {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9})
(let [regex (re/compile (str "(?=([1-9]|" (str/join "|"  (keys numbers)) "))"))
      digits (map #(let [matches (py->lisp (re/findall regex %))]
                     (for [v matches]
                       (if-let [d (get numbers v)] (str d) v)))
                  lines)

      nums (for [r digits]
             (int (str (first r) (last r))))]

  (reduce + nums))
;; => 55429

