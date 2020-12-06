(ns day6
  (:require [clojure.string :as str]))

(def test-input "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")

(defn part-one [input]
  (let [
        ;; Replace newlines with nothing to get all chars in group
        group-strs (map #(str/replace % "\n" "")
                        ;; Groups are separated by blank lines.
                        (str/split input
                                   #"\n\n"))]
    (reduce +
            (map (comp count set seq)
                 group-strs))))

(defn part-two [input]
  (let [group-data (map str/split-lines
                        (str/split input
                                   #"\n\n"))]
    (reduce (fn [acc group-strs]
              (let [group-count (count group-strs)]
                ;; If grouped count for char is equal to total in group
                ;; add to accumulated total
                (+ (count (filter (comp #{group-count} count second)
                                  ;; Group-by char
                                  (group-by identity
                                            ;; Get all chars in group strs
                                            (mapcat seq group-strs))))
                   acc)))
            0
            group-data)))

(part-one (slurp "inputs/day-six.txt"))
;; 6504
(part-two (slurp "inputs/day-six.txt"))
;; 3351