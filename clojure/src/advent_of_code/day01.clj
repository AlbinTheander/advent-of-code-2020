(ns advent-of-code.day01
  (:require [clojure.string :as str]))

(defn- part1 [nums]
  (apply * (first
   (for [x nums y nums :when (= 2020 (+ x y))] [x y]))))

(defn- part2 [nums]
  (apply * (first
            (for [x nums
                  y nums
                  z nums
                  :when (= 2020 (+ x y z))] [x y z]))))

(defn day01 []
  (let [data (slurp "../data/day01.txt")
        nums (map #(Integer/parseInt %) (str/split-lines data))
        answer1 (part1 nums)
        answer2 (part2 nums)]
    (println "===== Day 1 =====")
    (println "The product of the first pair is " answer1)
    (println "The product of the second triplet is " answer2)))

