(ns advent-of-code-2021.day-08
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split join]]
            [clojure.math.combinatorics :refer [permutations]]))

(defn read-line [str]
  (mapv #(split % #" ")
        (split str #" \| ")))

(defn read-input [file]
  (as-> file $
    (resource $) (slurp $) (split $ #"\n")
    (map read-line $)))

(def test-input (read-input "day_08_test_input.txt"))
(def input (read-input "day_08.txt"))

(defn part-1 [input]
  (->> input
       (mapv second)
       flatten
       (map count)
       (filter #{2 4 3 7})
       count))

(def digits {(set "cagedb")  0
             (set "ab")      1
             (set "gcdfa")   2
             (set "fbcad")   3
             (set "eafb")    4
             (set "cdfbe")   5
             (set "cdfgeb")  6
             (set "dab")     7
             (set "acedgfb") 8
             (set "cefabd")  9})

(def key-permutations (permutations "abcdefg"))

(defn translate [scrambled key]
  (->> scrambled
       (replace (zipmap key "abcdefg"))
       set
       digits))

(defn find-key [digits]
  (->> key-permutations
       (reduce (fn [_ key]
                 (if (every? #(translate % key) digits)
                   (reduced key)
                   nil))
               nil)
       join))

(defn part-2 [input]
  (->> input
       (reduce (fn [acc [a b]]
                 (let [key (find-key a)]
                   (conj acc (Integer/parseInt 
                              (join (map #(translate % key) b))))))
               [])
       (reduce +)))

#_(part-1 input) ;; => 344
#_(part-2 input) ;; => 1048410