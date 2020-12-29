(ns day10
  (:require [clojure.string :as str]))

(def test-input "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")

(defn parse [input]
  (map #(Long/parseLong %)
       (str/split-lines input)))

(defn difference [a b]
  (Math/abs (- a b)))

(defn part-one [input]
  (let [sorted-adapters (sort (parse input))
        highest-rated (last sorted-adapters)
        device-built-in-adapter (+ highest-rated 3)
        jolt-differences-map
        (group-by #(apply difference %)
                  (partition 2
                             1
                             (cons 0
                                   (concat sorted-adapters
                                           [device-built-in-adapter]))))]
    (* (count (get jolt-differences-map 1))
       (count (get jolt-differences-map 3)))))

(part-one (slurp "inputs/day-ten.txt"))

(defn part-two [input])

(part-two test-input)
