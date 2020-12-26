(ns day7
  (:require [clojure.string :as str]))

(def test-input "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.")

(defn parse-bag-str [s]
  (let [[[_ bag bag-contains-str]]
        (re-seq #"^(.*?) bag[s]{0,1} contain (\d.+)*"
                s)
        bag-contains (when (seq bag-contains-str)
                       (map (fn [bag-str]
                              (let [[_ count-str bag]
                                    (re-matches #".*([\d+]) (.*?) bag[s]{0,1}.*"
                                                bag-str)]
                                {:bag bag
                                 :count (Integer/parseInt count-str)}))
                            (str/split bag-contains-str
                                       #",")))]
    (merge {:bag bag}
           (when bag-contains
             {:bag-contains bag-contains}))))

(defn build-directed-graph [input]
  (let [lines (str/split-lines input)]
    (reduce (fn [acc line]
              (let [{:keys [bag bag-contains]}
                    (parse-bag-str line)]
                (update acc
                        bag
                        (fn [cur-state]
                          (reduce (fn [new-state inner-bag]
                                    (conj new-state
                                          inner-bag))
                                  cur-state
                                  bag-contains)))))
            {}
            lines)))

(defn dfs [graph root]
  (loop [stack (vector root)
         visited #{}
         path-traversed []]
    (if (empty? stack)
      path-traversed
      (let [current-vertex (or (get (peek stack) :bag)
                               (peek stack))
            neighbors (get graph current-vertex)
            not-visited (remove visited neighbors)
            new-stack (into (pop stack)
                            not-visited)]
        (if (get visited current-vertex)
          (recur new-stack
                 visited
                 path-traversed)
          (recur new-stack
                 (conj visited current-vertex)
                 (conj path-traversed current-vertex)))))))

(defn part-one [input contains-bag]
  (let [graph (build-directed-graph input)]
    (count (filter (fn [connections]
                     (some #{contains-bag} connections))
                   (map (fn [[vertex _neighbors]]
                          (dfs graph vertex))
                        (dissoc graph contains-bag))))))

(part-one (slurp "inputs/day-seven.txt")
          "shiny gold")

(def test-input-part-two "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags.")

(defn calculate-total-for-bag [graph bag]
  (let [bag-name (get bag :bag bag)
        contains (get graph bag-name)
        num-of-bag (get bag :count 1)]
    (if contains
      (+ num-of-bag
         (* num-of-bag
            (reduce +
                    0
                    (map (fn [contained-bag]
                           (calculate-total-for-bag graph
                                                    contained-bag))
                         contains))))
      num-of-bag)))

(defn part-two [input contains-bag]
  ;; One less to not count container bag itself
  (dec (calculate-total-for-bag (build-directed-graph input)
                                contains-bag)))

(part-two (slurp "inputs/day-seven.txt")
          "shiny gold")
