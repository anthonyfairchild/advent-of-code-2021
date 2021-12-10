(ns advent-of-code-2021.day-10
  (:require [clojure.string :refer [split]]
            [clojure.java.io :refer [resource]]
            [clojure.set :refer [map-invert]]))

(defn read-input [file]
  (as-> file $
    (resource $) (slurp $) (split $ #"\n")
    (map vec $)))

(def test-input (read-input "day_10_test_input.txt"))
(def input (read-input "day_10.txt"))

(def matching {\} \{ \] \[ \) \( \> \<})

(defn check-balance [line]
  (reduce (fn [stack c]
            (case c
              (\} \] \) \>) (if (= (matching c) (last stack))
                              (pop stack)
                              (reduced c))
              (\{ \( \[ \<) (conj stack c)))
          [] line))

(defn part-1 [input]
  (->> input
       (map check-balance)
       (filter #{\} \] \) \>})
       (map {\) 3 \] 57 \} 1197 \> 25137})
       frequencies
       (map (fn [[a b]] (* a b)))
       (reduce +)))

(defn part-2 [input]
  (->> input
       (map check-balance)
       (remove #{\} \] \) \>})
       (map reverse)
       (map #(map (map-invert matching) %))
       (map #(map {\) 1 \] 2 \} 3 \> 4} %))
       (map #(reduce (fn [total n] (+ (* total 5) n))
                     0 %))
       (sort)
       (#(nth % (/ (count %) 2)))))

#_(part-1 input) ;; => 265527
#_(part-2 input) ;; => 3969823589