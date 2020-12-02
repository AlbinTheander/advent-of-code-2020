(ns advent-of-code.day02
  (:require [clojure.string :as str]))

(defn- parseLine [s] 
  (let [[n1 n2 ch pw] (str/split s #"-|: | ")] 
    [(Integer/parseInt n1) (Integer/parseInt n2) (.charAt ch 0) pw]))

(defn- count-matching [xs pred]
  (->> xs
       (filter pred)
       count))

(defn- valid-password-1 [[n1 n2 ch pw]]
  (let [ch-ocurrences (count-matching (char-array pw) #(= ch %))]
    (<= n1 ch-ocurrences n2)))

(defn- valid-password-2 [[n1 n2 ch pw]]
  (not=
   (= (.charAt pw (dec n1)) ch)
   (= (.charAt pw (dec n2)) ch)))

(defn day02 []
  (let [data (slurp "../data/day02.txt")
        entries (map parseLine (str/split-lines data))
        answer1 (count-matching entries valid-password-1)
        answer2 (count-matching entries valid-password-2)
  ]
    (println "\n===== Day 2 =====")
    (println "The number of valid passwords with the old policy is" answer1)
    (println "The number of valid passwords with the new policy is" answer2)))