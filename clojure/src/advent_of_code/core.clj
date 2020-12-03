(ns advent-of-code.core
  (:require (advent-of-code [day01 :refer [day01]]
                            [day02 :refer [day02]]
                            [day03 :refer [day03]]))
  (:gen-class))

(defn -main []
  (day01)
  (day02)
  (day03))
