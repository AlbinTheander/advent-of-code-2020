(ns advent-of-code.day06
  (:require [clojure.string :as str]
            [clojure.set :refer [union, intersection]]))

(defn- sum-by [f xs]
  (apply + (map f xs)))

(defn- answers-by-anyone [group]
  (apply union group))

(defn- answers-by-everyone [group]
  (apply intersection group))

(defn day06 []
  (let [data (slurp "../data/day06.txt")
        groups (map #(map set (str/split-lines %)) (str/split data #"(?s)\n\n"))
        answer1 (sum-by count (map answers-by-anyone groups))
        answer2 (sum-by count (map answers-by-everyone groups))]
    (println "\n===== Day 6 =====")
    (println "Sum of answers by anyone in the group:" answer1)
    (println "Sum of answers by everyone in the group:" answer2)))

