(ns advent-of-code-2018.day2
  (:require [clojure.java.io :as io]))

(defn read-data
  [path]
  (with-open [rdr (io/reader (io/resource path))]
    (vec (line-seq rdr))))

;
; PART 1
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
; PART 2
;
; The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given the following box IDs:
;
; What letters are common between the two correct box IDs?
;

(defn diff-count
  [pair]
  (apply + (apply map #(if (= %1 %2) 0 1) pair)))

(defn find-off-by-one
  [path]
  (->> (read-data path)
       sort
       (map seq)
       (partition 2 1)
       (filter #(= 1 (diff-count %)))
       first
       (apply map #(if (= %1 %2) %1 nil))
       (apply str)))