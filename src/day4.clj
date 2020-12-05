(ns day4
  (:require [clojure.string :as str]))

(def test-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in")

(defn part-one [input required-fields]
  (let [passport-strs (str/split input
                                 #"\n\n")]
    (count (filter (fn [passport-str]
                     (let [fields (into #{}
                                        (map #(str/replace % #":" "")
                                             (re-seq #"byr:|iyr:|eyr:|hgt:|hcl:|ecl:|pid:|cid:"
                                                     passport-str)))]
                       (every? fields
                               required-fields)))
                   passport-strs))))

(defn parse [passport-str]
  (let [raw-fields (str/split passport-str
                              #"\s|\n")
        safe-parse-int (fn [s]
                         (try
                           (Integer/parseInt s)
                           (catch Exception _e)))
        field->parse-fn {"byr" safe-parse-int
                         "iyr" safe-parse-int
                         "eyr" safe-parse-int
                         "hgt" (fn [height-str]
                                 (let [[_ height-v-str height-unit]
                                       (re-matches #"(\d+)(in|cm)"
                                                   height-str)]
                                   {"value" (safe-parse-int height-v-str)
                                    "unit" height-unit}))
                         "hcl" identity
                         "ecl" identity
                         "pid" identity
                         "cid" identity}]
    (into {}
          (map (fn [field-str]
                 (let [[field raw-v]
                       (str/split field-str #":")]
                   [field
                    ((field->parse-fn field)
                     raw-v)]))
               raw-fields))))

(defn valid-passport? [{:strs [byr
                               iyr
                               eyr
                               hgt
                               hcl
                               ecl
                               pid
                               cid]}]
  (and
    (and byr
         (<= 1920 byr 2002))
    (and iyr
         (<= 2010 iyr 2020))
    (and eyr
         (<= 2020 eyr 2030))
    (and hgt
         (cond

           (= (get hgt "unit")
              "cm")
           (<= 150 (get hgt "value") 193)

           (= (get hgt "unit")
              "in")
           (<= 59 (get hgt "value") 76)))
    (and hcl
         (re-matches #"#[0-9a-f]{6}"
                     hcl))
    (and ecl
         (get #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
              ecl))
    (and pid
         (re-matches #"[0-9]{9}"
                     pid))))

(defn part-two [input]
  (let [passport-strs (str/split input
                                 #"\n\n")]
    (count (filter valid-passport?
                   (map parse
                        passport-strs)))))

