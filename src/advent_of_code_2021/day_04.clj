(ns advent-of-code-2021.day-04
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split trim]]))

(defn board-lines [board]
  (->> board (apply mapv hash-set) (concat (map set board)) vec))

(defn read-board [str]
  (-> str (split #"\n")
      (->> (mapv #(-> % trim (split #"\s+")
                      (->> (mapv read-string)))))
      board-lines))

(defn read-input [file]
  (let [things (-> file resource slurp (split #"\n\n"))]
    {:numbers (-> things first (split #",") (->> (map read-string)))
     :boards (->> things rest (map read-board))}))

(def test-input (read-input "day_04_test_input.txt"))

(def input (read-input "day_04.txt"))

(defn play-turn [n board]
  (let [new-board (map #(remove #{n} %) board)]
    {:win? (some empty? new-board) 
     :board new-board}))

(defn part-1 [{numbers :numbers boards :boards}]
  (reduce (fn [boards number]
            (let [results (map #(play-turn number %) boards)]
              (if-let [result (first (filter :win? results))]
                (reduced  (->> :board result flatten set (reduce +) (* number)))
                (map :board results))))
          boards numbers))

(defn part-2 [{numbers :numbers boards :boards}]
  (reduce (fn [boards number]
            (let [results (map #(play-turn number %) boards)]
              (if (and (= 1 (count results))
                       (:win? (first results)))
                (reduced (->> results first :board flatten set (reduce +) (* number)))
                (->> results (remove :win?) (map :board)))))
          boards numbers))

#_(part-1 input)
#_(part-2 input)