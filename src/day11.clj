(ns day11
  (:require [clojure.string :as str]))

(def test-input "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")

(defn parse [input]
  (let [rows (map seq (str/split-lines input))
        row-count (count rows)
        col-count (count (first rows))]
    {:row-count row-count
     :col-count col-count
     :seat-map (into {}
                     (apply concat
                            (map-indexed (fn [row-idx row]
                                           (map-indexed (fn [col-idx v]
                                                          [[row-idx col-idx]
                                                           (condp = v
                                                             \# :occupied
                                                             \L :empty
                                                             \. :floor)])
                                                        row))
                                         rows)))}))

(defn adjacent-seat-indices [seat-data [x y]]
  (remove (fn [[next-x next-y]]
            (or (< next-x 0)
                (< next-y 0)
                (>= next-x (get seat-data :row-count))
                (>= next-y (get seat-data :col-count))
                (and (= next-x x)
                     (= next-y y))))
          (for [x' [(dec x) x (inc x)]
                y' [(dec y) y (inc y)]]
            [x' y'])))

(defn adjacent-seat-states [seat-data seat]
  (let [seat-map (get seat-data :seat-map)
        adjacent-indices (adjacent-seat-indices seat-data seat)]
    (map (partial get seat-map)
         adjacent-indices)))

(defn next-seat-map [seat-data]
  (let [current-seat-map (get seat-data :seat-map)]
    (into {}
          (map (fn [[seat current-state]]
                 (if (= current-state :floor)
                   [seat :floor]
                   (let [adjacent-seat-states (adjacent-seat-states seat-data seat)
                         adjacent-occupied (count (filter #{:occupied}
                                                          adjacent-seat-states))]
                     (cond

                       (and (= current-state :empty)
                            (= adjacent-occupied 0))
                       [seat :occupied]

                       (and (= current-state :occupied)
                            (>= adjacent-occupied 4))
                       [seat :empty]

                       :default
                       [seat current-state]))))
               current-seat-map))))

(defn next-state [seat-data]
  (assoc seat-data
    :seat-map (next-seat-map seat-data)))

(defn part-one [input]
  (let [starting-state (parse input)
        equilibrium-state (->> (iterate next-state
                                        starting-state)
                               (partition 2)
                               (drop-while (fn [[prev-state cur-state]]
                                             (not= prev-state cur-state)))
                               (ffirst))]
    (count (filter (comp #{:occupied} second)
                   (get equilibrium-state :seat-map)))))

#_(part-one (slurp "inputs/day-eleven.txt"))


(defn seat-indices-in-line-of-sight [seat-data seat]
  (let [row-count (get seat-data :row-count)
        col-count (get seat-data :col-count)
        valid-x? (fn [x]
                   (and (>= x 0)
                        (< x row-count)))
        valid-y? (fn [y]
                   (and (>= y 0)
                        (< y col-count)))
        take-while-valid (fn [seats]
                           (take-while (fn [[x y]]
                                         (and (valid-x? x)
                                              (valid-y? y)))
                                       (rest seats)))]
    (concat
      (take-while-valid (iterate (fn [[x y]]
                                   [(dec x) (dec y)])
                                 seat))
      (take-while-valid (iterate (fn [[x y]]
                                   [(dec x) y])
                                 seat))
      (take-while-valid (iterate (fn [[x y]]
                                   [(dec x) (inc y)])
                                 seat))
      (take-while-valid (iterate (fn [[x y]]
                                   [x (dec y)])
                                 seat))
      (take-while-valid (iterate (fn [[x y]]
                                   [x (inc y)])
                                 seat))
      (take-while-valid (iterate (fn [[x y]]
                                   [(inc x) (dec y)])
                                 seat))
      (take-while-valid (iterate (fn [[x y]]
                                   [(inc x) y])
                                 seat))
      (take-while-valid (iterate (fn [[x y]]
                                   [(inc x) (inc y)])
                                 seat)))))

(defn line-of-sight-seat-states [seat-data seat]
  (let [seat-map (get seat-data :seat-map)
        line-of-sight-indices (seat-indices-in-line-of-sight seat-data seat)]
    (map (partial get seat-map)
         line-of-sight-indices)))

(defn next-seat-map-part-two [seat-data]
  (let [current-seat-map (get seat-data :seat-map)]
    (into {}
          (map (fn [[seat current-state]]
                 (if (= current-state :floor)
                   [seat :floor]
                   (let [line-of-sight-seat-states (line-of-sight-seat-states seat-data seat)
                         line-of-sight-occupied (count (filter #{:occupied}
                                                               line-of-sight-seat-states))]
                     (cond

                       (and (= current-state :empty)
                            (= line-of-sight-occupied 0))
                       [seat :occupied]

                       (and (= current-state :occupied)
                            (>= line-of-sight-occupied 5))
                       [seat :empty]

                       :default
                       [seat current-state]))))
               current-seat-map))))

(defn next-state-part-two [seat-data]
  (assoc seat-data
    :seat-map (next-seat-map-part-two seat-data)))

(defn part-two [input]
  (let [starting-state (parse input)
        equilibrium-state (->> (iterate next-state-part-two
                                        starting-state)
                               (partition 2)
                               (drop-while (fn [[prev-state cur-state]]
                                             (not= prev-state cur-state)))
                               (ffirst))]
    (count (filter (comp #{:occupied} second)
                   (get equilibrium-state :seat-map)))))

(count (filter (comp #{:occupied} second)
               (:seat-map (last (take 10 (iterate next-state-part-two
                                                 (parse test-input)))))))