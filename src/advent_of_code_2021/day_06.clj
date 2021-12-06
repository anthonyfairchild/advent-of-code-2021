(ns advent-of-code-2021.day-06
  (:require [clojure.string :refer [split]]
            [clojure.java.io :refer [resource]]))

(def input (-> "day_06.txt" resource slurp (split #",")
               (->> (map read-string))))

(defn cycle [state]
  (let [birth-count (get state 0 0)
        decremented (->> state
                         (map (fn [[n c]]
                                [(if (= -1 n) 6 (- n 1))
                                 c]))
                         (into {}))]
    (if (> birth-count 0)
      (as-> decremented $
        (remove #(= -1 (first %)) $)
        (into {} $)
        (update $ 6 #(+ (or % 0) birth-count))
        (assoc $ 8 birth-count))
      decremented)))

(defn part-1-and-2 [input days]
  (as-> input $
    (frequencies $)
    (reduce (fn [state _] (cycle state))
            $ (range days))
    (map second $)
    (reduce + $)))

#_(part-1-and-2 input 80)
#_(part-1-and-2 input 256)