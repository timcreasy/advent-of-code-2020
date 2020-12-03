(ns day2
  (:require [clojure.java.io :as io]))

(def test-input
  ["1-3 a: abcde"
   "1-3 b: cdefg"
   "2-9 c: ccccccccc"])

(defn parse [input]
  (let [[_ min-str max-str char-to-match-str password]
        (re-matches #"([\d]+)-([\d]+) ([A-Za-z]{1}): (.*)"
                    input)]
    {:min-position (Integer/parseInt min-str)
     :max-position (Integer/parseInt max-str)
     :char-to-match (.charAt char-to-match-str 0)
     :password password}))

(defn part-one [inputs]
  (count
    (filter (fn [{:keys [min-position
                         max-position
                         char-to-match
                         password]}]
              (<= min-position
                  (count (filter #{char-to-match}
                                 password))
                  max-position))
            (map parse inputs))))

(defn part-two [inputs]
  (count
    (filter (fn [{:keys [min-position
                         max-position
                         char-to-match
                         password]}]
              (let [min-match? (= (.charAt password (dec min-position))
                                  char-to-match)
                    max-match? (= (.charAt password (dec max-position))
                                  char-to-match)]
                (and (or min-match?
                         max-match?)
                     (not (and min-match? max-match?)))))
            (map parse inputs))))