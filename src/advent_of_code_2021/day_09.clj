(ns advent-of-code-2021.day-09
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split]]))

(defn read-input [file]
  (as-> file $
    (resource $) (slurp $) (split $ #"\n")
    (mapv #(mapv read-string (split % #"")) $)))

(def test-input (read-input "day_09_test_input.txt"))
(def input (read-input "day_09.txt"))

(defn neighbors [input [row col]]
  (->> [[0 1] [0 -1] [1 0] [-1 0]]
       (map (fn [[oy ox]] [(+ row oy) (+ col ox)]))
       (filter #(get-in input %))))

(defn all-positions [input]
  (->> input first count range
       (mapcat (fn [col] (map #(vector % col)
                           (range (count input)))))
       (into [])))

(defn low-points [input]
  (->> input
       all-positions
       (reduce (fn [acc [row col]]
                 (if (every? #(< (get-in input [row col]) %)
                             (map #(get-in input %)
                                  (neighbors input [row col])))
                   (conj acc [row col])
                   acc))
               [])))

(defn part-1 [input]
  (->> input
       low-points
       (map #(get-in input %))
       (map inc)
       (reduce +)))

(defn basin [input [row col]]
  (loop [b #{}
         pos-stack [[row col]]]
    (if (empty? pos-stack)
      b
      (let [[row col] (last pos-stack)]
        (if (not (b [row col]))
          (recur (conj b [row col])
                 (vec (concat (pop pos-stack)
                              (->> (neighbors input [row col])
                                   (remove #(= 9 (get-in input %)))))))
          (recur b (pop pos-stack)))))))

(defn part-2 [input]
  (->> input
       low-points
       (map #(basin input %))
       (map count)
       (sort >)
       (take 3)
       (reduce *)))

#_(part-1 input) ;; => 554
#_(part-2 input) ;; => 1017792