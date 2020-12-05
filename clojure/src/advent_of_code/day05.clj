(ns advent-of-code.day05
  (:require [clojure.string :as str]))

(defn- to-seat-nr
  ([s] (to-seat-nr 0 s))
  ([n s] (case (first s)
           nil n
           (\F \L) (to-seat-nr (* 2 n) (rest s))
           (to-seat-nr (+ 1 (* 2 n)) (rest s)))))

(defn- find-my-seat [seats]
  (let [is-taken (fn [seat] (some #(= % seat) seats))
        is-free (fn [seat]  (not (is-taken seat)))]
    (->> (range)
         (filter #(and (is-taken %) (is-free (+ % 1)) (is-taken (+ % 2))))
         (first))))


(defn day05 []
  (let [seats (->> "../data/day05.txt" (slurp) (str/split-lines) (map to-seat-nr))
        max-seat (reduce max seats)
        my-seat (find-my-seat seats)]
    (println "\n===== Day 5 =====")
    (println "The highest seat number is"  max-seat)
    (println "My seat number is" my-seat)))
