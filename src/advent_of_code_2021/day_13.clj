(ns advent-of-code-2021.day-13
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split]]))

(defn read-input [file]
  (let [[points-str folds-str] (as-> file $
                                 (resource $) (slurp $) (split $ #"\n\n"))
        points (as-> points-str $
                 (split $ #"\n")
                 (map #(split % #",") $)
                 (map (fn [[x y]] [(read-string x) (read-string y)]) $)
                 (into #{} $))
        folds (as-> folds-str $
                (split $ #"\n")
                (map #(let [[_ a b] (re-matches #"fold along (x|y)=(\d+)" %)]
                        [(read-string a) (read-string b)])
                     $))]
    [points folds]))

(def test-input (read-input "day_13_test_input.txt"))
(def input (read-input "day_13.txt"))

(defn step [points [axis n]]
  (->> points
       (map (fn [[x y]]
              (case axis
                x (if (> x n) [(- n (- x n)) y] [x y])
                y (if (> y n) [x (- n (- y n))] [x y]))))
       (into #{})))

(defn part-1 [[points folds]]
  (->> folds 
       (take 1)
       (reduce step points)
       count))

(defn print-board [points]
  (let [max-x (->> points (map first) (reduce max))
        max-y (->> points (map second) (reduce max))]
    (dotimes [y (inc max-y)]
      (dotimes [x (inc max-x)]
        (print (if (points [x y]) "# " ". ")))
      (println))
    (println)
    points))

(defn part-2 [[points folds]]
  (->> folds
       (reduce step points)
       print-board))

#_(part-1 input)
#_(part-2 input)
