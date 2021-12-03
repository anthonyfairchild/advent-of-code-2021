(ns advent-of-code-2021.day-01-1 (:require [clojure.string :refer [split]]
                                           [clojure.java.io :refer [resource]]))

(def input (-> "day_01.txt"
               resource
               slurp
               (split #"\n")
               (->> (map read-string))))

(def test-input [199 200 208 210 200 207 240 269 260 263])

(defn part-1
  [input]
  (->> input
       (partition 2 1)
       (filter #(apply < %))
       count))


(defn part-2
  [input]
  (->> input
       (partition 3 1)
       (map #(apply + %))
       (partition 2 1)
       (filter #(apply < %))
       count))

#_ input
#_ (part-1 input)       
#_ (part-2 input)