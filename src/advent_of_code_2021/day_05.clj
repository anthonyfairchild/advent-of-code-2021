(ns advent-of-code-2021.day-05
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split]]))

(defn read-input [file]
  (letfn [(read-pair [pair] (-> pair (split #",") (->> (mapv read-string))))
          (read-line [line] (-> line (split #" -> ") (->> (map read-pair))))]
    (-> file resource slurp (split #"\n")
        (->> (map read-line)))))

(def test-input (read-input "day_05_test_input.txt"))
(def input (read-input "day_05.txt"))

(defn lerp [a b t] (Math/round (+ a (* (float t) (- b a)))))

(defn line-points [[x1 y1] [x2 y2]]
  (let [num-points (max (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))]
    (map (fn [i] [(lerp x1 x2 (/ i num-points))(lerp y1 y2 (/ i num-points))])
         (range (+ 1 num-points)))))

(defn part-1 [input]
  (->> input
       (filter (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2))))
       (part-2)))

(defn part-2 [input]
  (->> input
       (map (fn [[a b]] (line-points a b)))
       (reduce (fn [acc line-points]
                 (reduce (fn [acc [x y]]
                           (update acc [x y] #(+ (or % 0) 1)))
                         acc line-points))
               {})
       (map second)
       (filter #(>= % 2))
       count))

#_(part-1 input)
#_(part-2 input)

;; (defn print-map [m]
;;   (let [rows (->> m (map first) (map second) (reduce max 0) (+ 1))
;;         cols (->> m (map first) (map first) (reduce max 0) (+ 1))]
;;     (dotimes [y rows]
;;       (dotimes [x cols]
;;         (print (or (m [x y]) \.)))
;;       (println))))
