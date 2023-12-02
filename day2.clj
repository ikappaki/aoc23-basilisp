(ns aoc23.day2
  (:require [clojure.string :as str]))

(def input (str/split-lines (slurp "aoc23/day2.1.input.aux")))

;; Part 1
;; Determine which games would have been possible 
;; if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes
(def bag {:red 12 :green 13 :blue 14})

(let [games 
      (reduce (fn [acc line]
                ;; line: "Game 1: 20 blue, 4 red; 2 red ..."
                (let [[game cubes] (str/split line #":")
                      ;; "cubes: 20 blue, 4 red; 2 red ..."
                      hands (str/split cubes #";")
                      ;; hands [["20 blue, 4 red"] ["2 red"] ...]
                      quants (for [hand hands]
                               (let [qs (str/split hand #",")
                                     ;; qs ["20 blue" " 4 red"]  
                                     qst (map str/trim qs)]
                                 (into {}
                                       (map #(let [[n c] (str/split % #" ")]
                                               [(keyword c) (int n)]) qst))))
                      ;; quants: [{:red 4 :blue 20} {:red 2} ...]
                      verdict (map #(every? (fn [[c q]] ;; [:red 4] 
                                              (<= q (get bag c)))
                                            %)
                                   quants)]
                  ;; verdict [(false true) ...]
                  (if (every? true? verdict)
                    (let [game-num (int (second (str/split game #" ")))]
                      (conj acc game-num))
                    acc)))
                    [] input)]
                    ;; games [0 1 ...]
                    (apply + games))
;; => 2476

;; Part 2
;; in each game you played, what is the fewest number of cubes of each color that could 
;; have been in the bag to make the game possible?
(let [games
      (reduce (fn [acc line]
                ;; line: "Game 1: 20 blue, 4 red; 2 red ..."
                (let [[game cubes] (str/split line #":")
                      ;; "cubes: 20 blue, 4 red; 2 red ..."
                      hands (str/split cubes #";")
                      ;; hands [["20 blue, 4 red"] ["2 red"] ...]
                      quants (for [hand hands]
                               (let [qs (str/split hand #",")
                                     ;; qs ["20 blue" " 4 red"]  
                                     qst (map str/trim qs)]
                                 (into {}
                                       (map #(let [[n c] (str/split % #" ")]
                                               [(keyword c) (int n)]) qst))))
                      ;; quants: [{:red 4 :blue 20} {:red 2} ...]
                      mx (reduce (fn [acc2 hand] ;; hand: {:red 4 :blue 20}
                                   (reduce (fn [acc3 [c q]] ;; [:red 4]
                                             (if (> q (get acc3 c))
                                               (assoc acc3 c q)
                                               acc3))
                                           acc2 hand))
                                 {:red 0 :blue 0 :green 0} quants)
                      ;; mx: {:red 4 :blue 20 ...}
                      res (* (:red mx) (:green mx) (:blue mx))]
                  (conj acc res)))
              [] input)] 
  (apply + games))
;; => 54911
