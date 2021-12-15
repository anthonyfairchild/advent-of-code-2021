(ns advent-of-code-2021.day-12
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split upper-case]]))

(defn read-input [file]
  (as-> file $
    (resource $) (slurp $) (split $ #"\n")
    (map #(split % #"-") $)
    (map (fn [[a b]] [[a b] [b a]]) $)
    (mapcat seq $)
    (group-by first $)
    (map (fn [[k vs]] [k (mapv second vs)]) $)
    (into {} $)))

(def test-input-1 (read-input "day_12_test_input_1.txt"))
(def test-input-2 (read-input "day_12_test_input_2.txt"))
(def input (read-input "day_12.txt"))

#_test-input-2

(defn part-1 [input]
  (loop [v #{}
         s ["start"]
         c 0]
    (if (empty? s)
      c
      (let [k (last s)
            s (pop s)]
        (cond
          (= k "end")                (recur v s (inc c))
          (= k (upper-case k))       (recur v (vec (concat s (get input k))) c)
          (not (v k))                (recur (conj v k) (vec (concat s (get input k))) c)
          :else                      (recur v s c))))))


#_(part-1 test-input-1)