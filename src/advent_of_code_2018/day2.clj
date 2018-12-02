(ns advent-of-code-2018.day2
  (:require [clojure.java.io :as io]))

(defn read-data
  [path]
  (with-open [rdr (io/reader (io/resource path))]
    (vec (line-seq rdr))))

;
; Part 1
;

(defn checksum
  [path]
  (let [counts (->> (read-data path)
                    (map #(group-by identity %))
                    (map #(map (fn [x] (count (second x))) %))
                    (map #(into #{} %)))
        twos (count (filter #(some #{2} %) counts))
        threes (count (filter #(some #{3} %) counts))]
    (* twos threes)))

;
; Part 2
;
; The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given the following box IDs:
;
; What letters are common between the two correct box IDs?
;

;
; Calculate Levenshtein distance between strings.
; Re-used code from solution http://www.4clojure.com/problem/101
;
(defn lev-dist
  ([s t a]
   (let [key (into #{} [s t])
         cached (get @a key)]
     (or
       cached
       (let [count-s (count s)
             count-t (count t)
             dist (if (zero? (min count-s count-t))
                    (max count-s count-t)
                    (let [trunc-s (drop-last s)
                          trunc-t (drop-last t)
                          cost (if (= (last s) (last t)) 0 1)]
                      (min (inc (lev-dist trunc-s t a))
                           (inc (lev-dist s trunc-t a))
                           (+ (lev-dist trunc-s trunc-t a) cost))))]
         (get (swap! a assoc key dist) key)))))
  ([s t]
   (lev-dist s t (atom {}))))

(defn find-off-by-one
  [path]
  (let [memo (atom {})]
    (->> (read-data path)
         sort
         (partition 2 1)
         (map vec)
         (map #(vector % (apply lev-dist (conj % memo))))
         (filter #(= 1 (second %)))
         ffirst
         (map seq)
         (apply map #(if (= %1 %2) %1 nil))
         (apply str))))