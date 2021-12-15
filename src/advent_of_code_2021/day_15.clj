(ns advent-of-code-2021.day-15
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split]]))

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
  (->> [[-1 0][0 -1][0 1][1 0]]
       (map (fn [[ox oy]] [(+ ox x) (+ oy y)]))
       (filter (fn [[x y]]
                 (and (>= x 0) (>= y 0) (< x (count (first input))) (< y (count input)))))))


(defn part-1 [input]
  (let [target [(dec (count (first input)))
                (dec (count input))]
        [q dist prev] (reduce (fn [[q dist prev] v]
                                  [(conj q v)
                                   (conj dist {v ##Inf})
                                   (conj prev {v nil})])
                                [#{} {} {}] (all-positions input))
         dist (conj dist [0 0])]
      #dbg(loop [q q
             dist dist
             prev prev]
        #break 
        (if (empty? q)
          [dist prev]
          (let [[x y :as u] (first (sort-by second q))
                q (disj q u)]
            (if (= target u)
              [:done target prev]
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