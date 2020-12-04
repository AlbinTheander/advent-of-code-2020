(ns advent-of-code.day04
  (:require [clojure.string :as str]))

(defn- parsePassports [s]
  (as-> s x
    (str/split x #"(?s)\n\n")
    (map #(str/split % #"(?s)[:\s]+") x)
    (map #(apply hash-map %) x)))

(def mandatory-fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

(defn- complete? [passport]
  (every? #(contains? passport %) mandatory-fields))

(defn- is-valid-year [year from to]
  (and (re-matches #"^[0-9]{4}$" year)
       (<= from (Integer. year) to)))

(defn- valid-height? [hgt]
  (let [[_ h unit] (re-matches #"^([0-9]{2,3})(cm|in)" hgt)]
    (cond
      (= unit "cm") (<= 150 (Integer. h) 193)
      (= unit "in") (<= 59 (Integer. h) 76)
      :else false)))

(defmulti valid-field? first)
(defmethod valid-field? "byr" [[_ year]] (is-valid-year year 1920 2002))
(defmethod valid-field? "iyr" [[_ year]] (is-valid-year year 2010 2020))
(defmethod valid-field? "eyr" [[_ year]] (is-valid-year year 2020 2030))
(defmethod valid-field? "hgt" [[_ hgt]] (valid-height? hgt))
(defmethod valid-field? "hcl" [[_ col]] (re-matches #"^#[0-9a-f]{6}$" col))
(defmethod valid-field? "ecl" [[_ col]] (some #(= col %) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))
(defmethod valid-field? "pid" [[_ pid]] (re-matches #"^[0-9]{9}$" pid))
(defmethod valid-field? :default [_] true)

 

(defn- valid? [passport]
  (every? valid-field? passport))

(defn day04 []
  (let [data (slurp "../data/day04.txt")
        passports (parsePassports data)
        complete-passports (filter complete? passports)
        valid-passports (filter valid? complete-passports)]
    (println "\n===== Day 4 =====")
    (println "There are" (count complete-passports) "complete passports")
    (println "There are" (count valid-passports) "valid passports")))
