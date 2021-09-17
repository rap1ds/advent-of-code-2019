(ns Projects.own.advent-of-code-2019.day4
  (:require [clojure.set :as set]))

(def r (clojure.core/range 156218 652528))

(def digits
  (map #(map (fn [n] (Character/digit n 10)) (str %))))

#_(defn two-adjacent-same? [n]
  (boolean
   (some (fn [[a b]] (= a b)) (partition 2 1 (digits n)))))

#_(defn not-decreases? [n]
  (boolean
   (every? (fn [[a b]] (<= a b)) (partition 2 1 (digits n)))))

(defn pairs [dgs]
  (partition 2 1 dgs))

(defn triples [dgs]
  (partition 3 1 dgs))

(def two-adjacent-same?
  (filter #(some (fn [[a b]] (= a b)) (pairs %))))

(def not-decreases?
  (filter #(every? (fn [[a b]] (<= a b)) (pairs %))))

(def exactly-two-same?
  (filter
   #(let [two-same (set (map first (filter (fn [[a b]] (= a b)) (pairs %))))
          three-same (set (map first (filter (fn [[a b c]] (= a b c)) (triples %))))]
      (seq (set/difference two-same three-same)))))

(comment
  ;; part 1
  (count
   (into []
         (comp
          digits
          not-decreases?
          two-adjacent-same?)
         r))

  ;; part 2
  (count
   (into []
         (comp
          digits
          not-decreases?
          two-adjacent-same?
          exactly-two-same?)
         r))
  )
