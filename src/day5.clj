(ns day5
  (:require [clojure.string :as str]))

(def test-input "FBBBBBBRRL\nBBFFFBBLRL\nFBFFBFFLRL\nFFBBBFBLRR\nFFBFBFFLLL\nBFBBBBFLLL\nFFBFFFFLLR\nFBBFBFBLRR\nBBBFFFFRRR\nBFBFFFBLLR\nBFFBBFBLLL\nFBBFFBFLRL\nFBBBFBFRLR")

(defn get-row [seat-str total-rows]
  (let [row-instructions (take 7 seat-str)]
    (first
      (reduce (fn [remaining-rows-to-consider instruction]
                (let [[first-half second-half]
                      (split-at (/ (count remaining-rows-to-consider)
                                   2)
                                remaining-rows-to-consider)]
                  (if (= instruction \F)
                    first-half
                    second-half)))
              (range total-rows)
              row-instructions))))

(defn get-seat [seat-str total-seats-in-row]
  (let [seat-instructions (seq (subs seat-str 7))]
    (first
      (reduce (fn [remaining-seats-to-consider instruction]
                (let [[first-half second-half]
                      (split-at (/ (count remaining-seats-to-consider)
                                   2)
                                remaining-seats-to-consider)]
                  (if (= instruction \L)
                    first-half
                    second-half)))
              (range total-seats-in-row)
              seat-instructions))))

(defn get-seat-id
  ([row seat]
   (+ (* row 8)
      seat))
  ([total-rows total-seats-in-row seat-str]
   (let [row (get-row seat-str total-rows)
         seat (get-seat seat-str total-seats-in-row)]
     (get-seat-id row seat))))

(defn part-one [input]
  (let [total-rows 128
        seats-in-row 8
        seat-strs (str/split-lines input)]
    (apply max
           (map (partial get-seat-id total-rows seats-in-row)
                seat-strs))))

(defn part-two [input]
  (let [total-rows 128
        seats-in-row 8
        seat-strs (str/split-lines input)
        sorted-seat-ids (sort (map (partial get-seat-id total-rows seats-in-row)
                                   seat-strs))]
    (inc (ffirst (remove (fn [[s1 s2]]
                           (= (inc s1) s2))
                         (partition-all 2 1 sorted-seat-ids))))))