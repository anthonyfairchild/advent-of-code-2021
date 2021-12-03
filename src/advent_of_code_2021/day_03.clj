(ns advent-of-code-2021.day-03
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split]]))

(def input (-> "day_03.txt"
               resource
               slurp
               (split #"\n")
               (->> (map (fn [ds] (map #(Character/getNumericValue %) ds))))))

(def test-input [[0 0 1 0 0] [1 1 1 1 0] [1 0 1 1 0] [1 0 1 1 1]
                 [1 0 1 0 1] [0 1 1 1 1] [0 0 1 1 1] [1 1 1 0 0]
                 [1 0 0 0 0] [1 1 0 0 1] [0 0 0 1 0] [0 1 0 1 0]])

(defn acc-digit [acc n]
  (update acc n #(+ (or % 0) 1)))

(defn acc-row [acc row]
  (->> (mapv vector acc row)
       (map #(apply acc-digit %))))

(defn bits2num [bits]
  (reduce (fn [a d] (+ d (* a 2)))
          0
          bits))

(defn digit-counts [input]
  (reduce acc-row
          (repeat (count (first input)) {})
          input))

(defn least-common-digit [{zeros 0 ones 1 :or {zeros 0 ones 0}}]
  (if (>= ones zeros) 0 1))

(defn most-common-digit [{zeros 0 ones 1 :or {zeros 0 ones 0}}]
  (if (>= ones zeros) 1 0))

(defn part-1 [input]
  (let [digit-counts (digit-counts input)
        gamma (->> digit-counts
                   (map most-common-digit)
                   bits2num)
        epsilon (->> digit-counts
                     (map least-common-digit)
                     bits2num)]
    (* gamma epsilon)))

(defn col-digit-counts [input icol]
  (->> input
       (map #(nth % icol))
       (reduce acc-digit {})))

(defn filter-column [n icol input]
  (filter #(= n (nth % icol)) input))

(defn part-2 [input]
  (letfn [(rating [fun] (->> (range (count (first input)))
                             (reduce (fn [acc icol]
                                       (if (> (bounded-count 2 acc) 1)
                                         (-> acc
                                             (col-digit-counts icol)
                                             fun
                                             (filter-column icol acc))
                                         acc))
                                     input)
                             first
                             bits2num))]
    (* (rating most-common-digit)
       (rating least-common-digit))))

#_(part-1 input)
#_(part-2 input)

