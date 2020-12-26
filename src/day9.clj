(ns day9
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def test-input "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576")

(defn parse [input]
  (map #(Long/parseLong %)
       (str/split-lines input)))

(parse test-input)

(defn part-one [input preamble-size n-to-sum]
  (let [parsed-input (parse input)]
    (reduce (fn [_ group]
              (let [[preamble [sum-to-find]]
                    ((juxt (partial take preamble-size)
                           (partial drop preamble-size))
                     group)
                    sums-in-preamble (into #{}
                                           (map (partial apply +)
                                                (combo/combinations preamble
                                                                    n-to-sum)))]
                (when-not (get sums-in-preamble sum-to-find)
                  (reduced sum-to-find))))
            (partition (inc preamble-size)
                       1
                       parsed-input))))

(part-one (slurp "inputs/day-nine.txt")
          25
          2)

(defn part-two [input preamble-size n-to-sum]
  (let [invalid-number (part-one input preamble-size n-to-sum)
        parsed-input (parse input)
        contiguous-nums-summing-to-invalid-number
        (reduce (fn [_ group]
                  (when (= invalid-number
                           (apply + group))
                    (reduced group)))
                (mapcat (fn [partition-size]
                          (partition partition-size
                                     1
                                     parsed-input))
                        (range 2 1000)))]
    (+ (apply min contiguous-nums-summing-to-invalid-number)
       (apply max contiguous-nums-summing-to-invalid-number))))


(part-two (slurp "inputs/day-nine.txt")
          25
          2)