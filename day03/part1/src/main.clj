(ns main)
(use '[clojure.string :as s])

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def priorities (zipmap (concat (char-range \a \z) (char-range \A \Z)) (range 1 53)))

(defn common-item-type [first-compartment second-compartment]
  (if (s/includes? second-compartment (str(first first-compartment)))
    (first first-compartment)
    (recur (rest first-compartment) second-compartment)))

(defn sum-priorities [rucksack-list accumulator]
  (if (empty? rucksack-list)
    accumulator
    (let [current-rucksack (first rucksack-list)]
      (let [first-compartment (subs current-rucksack 0 (/ (count current-rucksack) 2)) second-compartment (subs current-rucksack (/ (count current-rucksack) 2))]
        (recur (rest rucksack-list) (+ accumulator (priorities(common-item-type first-compartment second-compartment))))))))

(defn -main [& args]
  (let [input (s/split-lines (slurp "./input.txt"))]
    (println (sum-priorities input 0))))
