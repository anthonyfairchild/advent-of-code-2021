(ns advent-of-code-2021.day-15
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split]]
            [clojure.set :refer [map-invert]]))

(defn read-input [file]
  (as-> file $
    (resource $) (slurp $) (split $ #"\n")
    (mapv (fn [ds] (mapv #(Character/getNumericValue %) ds)) $)))

(def test-input (read-input "day_15_test_input.txt"))
(def input (read-input "day_15.txt"))

#_test-input

(defn all-positions [input]
  (->> input first count range
       (mapcat (fn [col] (map #(vector % col)
                              (range (count input)))))
       (into [])))

#_(all-positions test-input)

(defn adjacents [input [x y]]
  (->> [[-1 0] [0 -1] [0 1] [1 0]]
       (map (fn [[ox oy]] [(+ ox x) (+ oy y)]))
       (filter (fn [[x y]]
                 (and (>= x 0) (>= y 0) (< x (count (first input))) (< y (count input)))))))

(defn make-path [input target prev]
  (let [prev (map-invert prev)
        path (loop [curr [0 0]
                    path []]
               (let [c (prev curr)]
                 (if c
                   (recur c (conj path c))
                   path)))]
    (->> path
         #_(map (fn [[x y]] (get-in input [y x]))))))

#_(make-path test-input [9 9] {[8 8] nil, [7 6] nil, [8 7] nil, [9 8] nil, [7 1] nil, [8 9] nil, [4 3] nil, [2 2] [2 1], [0 0] nil, [3 9] [3 8], [7 7] nil, [2 8] [2 7], [1 0] [0 0], [8 4] nil, [2 3] [2 2], [2 5] [2 4], [7 2] nil, [6 7] nil, [7 4] nil, [8 3] nil, [0 6] [0 5], [3 3] [3 2], [5 4] nil, [1 1] [1 0], [6 3] nil, [0 5] [0 4], [3 4] [3 3], [7 3] nil, [8 6] nil, [4 2] nil, [7 8] nil, [3 0] nil, [9 0] nil, [6 6] nil, [9 6] nil, [1 9] [1 8], [5 3] nil, [9 9] [9 9], [9 3] nil, [4 7] [4 6], [4 9] [3 9], [2 9] [1 9], [6 5] nil, [0 9] [0 8], [8 0] nil, [4 1] nil, [5 2] nil, [4 6] [4 5], [1 4] [1 3], [5 7] [5 6], [8 2] nil, [1 3] [1 2], [4 8] [4 7], [1 5] [1 4], [1 8] [1 7], [1 7] [1 6], [6 4] nil, [8 1] nil, [0 3] [0 2], [5 1] nil, [6 1] nil, [5 6] [4 6], [5 8] [5 7], [8 5] nil, [0 7] [0 6], [6 8] [5 8], [5 5] nil, [7 9] nil, [2 7] [1 7], [5 9] [5 8], [2 4] [1 4], [3 6] [3 5], [9 2] nil, [4 5] [4 4], [9 1] nil, [9 7] nil, [7 0] nil, [0 2] [0 1], [6 9] [6 8], [2 0] [1 0], [0 4] [0 3], [3 1] nil, [2 1] [1 1], [9 5] nil, [3 8] [3 7], [9 4] nil, [1 6] [0 6], [4 4] [3 4], [3 7] [2 7], [7 5] nil, [2 6] [1 6], [5 0] nil, [6 2] nil, [6 0] nil, [1 2] [0 2], [3 5] [2 5], [0 8] [0 7], [3 2] [2 2], [0 1] [0 0], [4 0] nil})

(defn part-1 [input]
  (let [target [(dec (count (first input)))
                (dec (count input))]
        [q dist prev] (reduce (fn [[q dist prev] v]
                                [(conj q v)
                                 (conj dist {v Integer/MAX_VALUE})
                                 (conj prev {v nil})])
                              [#{} {} {}] (all-positions input))
        dist (conj dist {[0 0] 0})]
    (loop [q q
           dist dist
           prev prev]
      (if (empty? q)
        nil
        (let [[x y :as u] (first (sort-by second q))
              q (disj q u)]
          (if (= target u)
            (make-path input target prev)
            (let [[q dist prev] (->> (adjacents input u)
                                     (filter q)
                                     (reduce (fn [[q dist prev] v]
                                               (let [alt (+ (dist u) (get-in input [y x]))]
                                                 (if (< alt (dist v))
                                                   [q (conj dist {v alt}) (conj prev {v u})]
                                                   [q dist prev])))
                                             [q dist prev]))]
              (recur q dist prev))))))))

#_(part-1 test-input)