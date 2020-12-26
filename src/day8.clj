(ns day8
  (:require [clojure.string :as str]))

(def test-input "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6")

(defn parse [input]
  (loop [instruction-strs (str/split-lines input)
         line-number 1
         parsed-instructions []]
    (if-let [instruction-str (first instruction-strs)]
      (let [[_ op sign-str value-str]
            (re-matches #"^(nop|jmp|acc) (\+|-)(\d+)"
                        instruction-str)]
        (recur (rest instruction-strs)
               (inc line-number)
               (conj parsed-instructions
                     {:line line-number
                      :op op
                      :sign (resolve (symbol sign-str))
                      :value (Long/parseLong value-str)})))
      parsed-instructions)))

(defn part-one [input]
  (let [parsed-instructions (parse input)]
    (loop [acc 0
           current-line 1
           executed-lines #{}]
      ;; Per instructions, if line has been executed before
      ;; we have an infinite loop.  Short circuit with acc value
      (if (get executed-lines current-line)
        acc
        (let [{:keys [op sign value]}
              ;; 0-indexed
              (get parsed-instructions (dec current-line))]
          (recur (if (= op "acc")
                   (sign acc value)
                   acc)
                 (if (= op "jmp")
                   (sign current-line value)
                   (inc current-line))
                 (conj executed-lines current-line)))))))

(part-one (slurp "inputs/day-eight.txt"))

(defn termination-value-or-false [instructions]
  (loop [acc 0
         current-line 1
         executed-lines #{}]
    ;; Per instructions, if line has been executed before
    ;; we have an infinite loop.  Short circuit with acc value
    (if (get executed-lines current-line)
      false
      ;; We executed the last line
      (if (> current-line (count instructions))
        acc
        (let [{:keys [op sign value]}
              ;; 0-indexed
              (get instructions (dec current-line))]
          (recur (if (= op "acc")
                   (sign acc value)
                   acc)
                 (if (= op "jmp")
                   (sign current-line value)
                   (inc current-line))
                 (conj executed-lines current-line)))))))

(defn part-two [input]
  (let [parsed-instructions (parse input)]
    (loop [current-line-to-modify 1]
      (let [updated-instructions (update parsed-instructions
                                         (dec current-line-to-modify)
                                         (fn [instruction-at-line]
                                           (let [current-op (get instruction-at-line :op)]
                                             (condp = current-op
                                               "jmp" (assoc instruction-at-line
                                                       :op "nop")
                                               "nop" (assoc instruction-at-line
                                                       :op "jmp")
                                               instruction-at-line))))]
        (if-let [termination-value (termination-value-or-false updated-instructions)]
          termination-value
          (recur (inc current-line-to-modify)))))))

(part-two (slurp "inputs/day-eight.txt"))
