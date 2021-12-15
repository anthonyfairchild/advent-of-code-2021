(ns advent-of-code-2021.day-11
  (:require [clojure.string :refer [split]]
            [clojure.java.io :refer [resource]]))

(defn read-input [file]
  (as-> file $
    (resource $) (slurp $) (split $ #"\n")
    (mapv (fn [ds] (mapv #(Character/getNumericValue %) ds)) $)))

(def test-input (read-input "day_11_test_input.txt"))
(def input (read-input "day_11.txt"))

(defn adjacents [input [x y]]
  (->> [[-1 -1] [-1 0] [-1 1]
        [0 -1]        [0 1]
        [1 -1] [1 0] [1 1]]
       (map (fn [[ox oy]] [(+ ox x) (+ oy y)]))
       (filter (fn [[x y]]
                 (and (>= x 0) (>= y 0) (< x (count (first input))) (< y (count input)))))))

(defn all-positions [input]
  (->> input first count range
       (mapcat (fn [col] (map #(vector % col)
                              (range (count input)))))
       (into [])))

(defn flash-all [input]
  (loop [input input
         positions (all-positions input)
         flashed #{}]
    (if (empty? positions)
      input
      (let [[x y] (last positions)
            n (get-in input [y x])
            adj (adjacents input [x y])]
        (if (and (>= n 10) (not (flashed [x y])))
          (as-> adj $
            (reduce (fn [input [x y]]
                      (update-in input [y x] inc)) input $)
            (recur $ (vec (concat (pop positions) adj)) (conj flashed [x y])))
          (recur input (pop positions) flashed))))))

(defn step [input]
  (->> input
       (mapv (fn [row] (mapv inc row)))
       flash-all
       (mapv (fn [row] (mapv #(if (> % 9) 0 %) row)))))

(defn flash-count [input]
  (->> input flatten (filter #(= % 0)) count))

(defn part-1 [input]
  (->> (range 100)
       (reduce (fn [[fc input] _]
                 (let [s (step input)]
                   [(+ fc (flash-count s))
                    s]))
               [0 input])
       first))

(defn part-2 [input]
  (loop [n 1
         input input]
    (let [s (step input)]
      (if (= 100 (flash-count s))
        n
        (recur (inc n) s)))))

#_(part-1 input)
#_(part-2 input)