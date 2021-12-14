(ns advent-of-code-2021.day-14
  (:require [clojure.string :refer [split]]
            [clojure.java.io :refer [resource]]))

(defn read-input [file]
  (let [[template-str rules-str] (as-> file $
                                   (resource $) (slurp $) (split $ #"\n\n"))
        template (->> template-str
                      (partition 2 1)
                      (frequencies)
                      (seq))
        last-letter (last template-str)
        rules (as-> rules-str $
                (split $ #"\n")
                (map #(split % #" -> ") $)
                (map (fn [[k v]] [(seq k) (first v)]) $)
                (into {} $))]
    [template rules last-letter]))

(def test-input (read-input "day_14_test_input.txt"))
(def input (read-input "day_14.txt"))

(defn step [[template rules]]
  (reduce (fn [template [[a b] n]]
            (let [c (rules [a b])]
              (-> template
                  (update [a c] #(+ (or % 0) n))
                  (update [c b] #(+ (or % 0) n)))))
          {} template))

(defn part-1-2 [[template rules last-letter] steps]
  (let [sums (as-> steps $
                   (range $)
                   (reduce (fn [template _] (step [template rules]))
                           template $)
                   (reduce (fn [freq [[a _] n]]
                             (-> freq
                                 (update a #(+ (or % 0) n))))
                           {} $)
                   (update $ last-letter inc)
                   (map second $))
        max (reduce max sums)
        min (reduce min sums)]
    (- max min)))

#_(part-1-2 input 10)
#_(part-1-2 input 40)
