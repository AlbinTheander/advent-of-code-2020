(ns advent-of-code.day03
  (:require [clojure.string :as str]))

(defn- height [terrain] (count terrain))
(defn- width [terrain] (-> terrain first count))

(defn- get-spot [terrain x y]
  (-> terrain
      (nth y)
      (nth (mod x (width terrain)))))

(defn- coords [dx dy height]
  (loop [x 0 y 0 coords []]
    (if (< y height) 
      (recur (+ x dx) (+ y dy) (cons [x y] coords))
      coords)))

(defn- count-trees [terrain dx dy]
  (->> terrain
       height
       (coords dx dy)
       (map (fn [[x y]] (get-spot terrain x y)))
       (filter #(= % \#))
       (count)))

(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn day03 []
  (let [data (slurp "../data/day03.txt")
        terrain (str/split-lines data)
        answer1 (count-trees terrain 3 1)
        answer2 (apply * (map (fn [[dx dy]] (count-trees terrain dx dy)) slopes))]
    (println "\n===== Day 3 =====")
    (println "I hit" answer1 "trees on the way down")
    (println "Trying more paths, the collisions gave me the magic number" answer2)))

