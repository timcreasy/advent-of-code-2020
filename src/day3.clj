(ns day3
  (:require [clojure.string :as str]))

(defn part-one [input start-coord tree-char slope]
  (let [rows (str/split-lines input)
        row-length (count (first rows))
        infinite-map-seqs (map repeat
                               rows)
        [right down] slope
        instructions (take-while (fn [[_x y]]
                                   (< y (count rows)))
                                 (iterate (fn [[x y]]
                                            [(+ x right)
                                             (+ y down)])
                                          start-coord))]
    (reduce (fn [tree-count [x y]]
              (let [row-seqs (nth infinite-map-seqs y)
                    ;; Find how many seqs to join together for row string
                    ;; based on how far the x position is
                    num-seqs-to-join (inc
                                       (if (= 0 x)
                                         1
                                         (int (Math/ceil (/ x row-length)))))
                    row-str (apply str (take num-seqs-to-join
                                             row-seqs))]
                (if (= (.charAt row-str x)
                       tree-char)
                  (inc tree-count)
                  tree-count)))
            0
            instructions)))

(defn part-two [input start-coord tree-char]
  (reduce *
          (map (partial part-one input start-coord tree-char)
               [[1 1]
                [3 1]
                [5 1]
                [7 1]
                [1 2]])))