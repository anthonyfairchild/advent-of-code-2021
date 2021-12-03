(ns advent-of-code-2021.day-02
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split]]))

(def input (-> "day_02.txt"
               resource
               slurp
               (split #"\n")
               (->> (map #(mapv read-string (split % #" "))))))

(def test-input [['forward 5]
                 ['down 5]
                 ['forward 8]
                 ['up 3]
                 ['down 8]
                 ['forward 2]])

(defn part-1
  [input]
  (let [[x y] (reduce (fn [[x y] [d n]]
                        (case d
                          forward [(+ x n) y]
                          down    [x (+ y n)]
                          up      [x (- y n)]))
                      [0 0] input)]
    (* x y)))

(defn part-2
  [input]
  (let [[x y] (reduce (fn [[x y a] [d n]]
                        (case d
                          forward [(+ x n) (+ y (* a n)) a]
                          down    [x y (+ a n)]
                          up      [x y (- a n)]))
                      [0 0 0] input)]
    (* x y)))

#_input
#_(part-1 input)
#_(part-2 input)