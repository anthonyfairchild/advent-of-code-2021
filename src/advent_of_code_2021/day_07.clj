(ns advent-of-code-2021.day-07 
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split]]))

(def input (as-> "day_07.txt" $
             (resource $) (slurp $) (split $ #",")
             (mapv read-string $)))

(def test-input [16,1,2,0,4,2,7,1,2,14])

(defn fuel [input target]
  (reduce #(+ %1 (Math/abs (- %2 target)))
          0 input))

(defn best-fuel [input fuel-fun]
  (let [max-depth (inc (reduce max input))]
    (reduce (fn [best target]
              (let [f (fuel-fun input target)]
                (if (or (nil? best)
                        (< f best))
                  f
                  best)))
            nil
            (range max-depth))))

(defn part-1 [input]
  (best-fuel input fuel))

(defn fuel-2 [input target]
  (reduce #(+ %1 (let [n (Math/abs (- %2 target))]
                   (/ (* n (+ n 1)) 2)))
          0 input))

(defn part-2 [input]
  (best-fuel input fuel-2))

#_(part-1 input) ;; => 337488
#_(part-2 input) ;; => 89647695
