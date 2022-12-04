(ns main)
(use '[clojure.string :as s])

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn third [coll]
  (nth coll 2))

(def priorities (zipmap (concat (char-range \a \z) (char-range \A \Z)) (range 1 53)))

(defn common-item-type [elve-group]
  (let [first-rucksack (first elve-group) second-rucksack (second elve-group) third-rucksack (third elve-group)]
    (if (and (s/includes? second-rucksack (str (first first-rucksack))) (s/includes? third-rucksack (str (first first-rucksack))))
      (first first-rucksack)
      (recur (list (rest first-rucksack) second-rucksack third-rucksack)))))

(defn sum-priorities-group [rucksack-list accumulator]
  (if (empty? rucksack-list)
    accumulator
    (let [current-group (take 3 rucksack-list)]
        (recur (drop 3 rucksack-list) (+ accumulator (priorities(common-item-type current-group)))))))

(defn -main [& args]
  (let [input (s/split-lines (slurp "./input.txt"))]
    (println (sum-priorities-group input 0))))
