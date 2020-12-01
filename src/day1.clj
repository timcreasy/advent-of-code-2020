(ns day1
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(defn parse [file-name]
  (->> (io/reader file-name)
       line-seq
       (map #(Integer/parseInt %))))

(defn day-one [input n-entries sum-to-match]
  "Gets unique combinations of n-entries number of items from input
  and finds which set of n-entries items matches sum-to-match"
  (some (fn [pair]
          (when (= (apply + pair)
                   sum-to-match)
            (apply * pair)))
        (combo/combinations input
                            n-entries)))