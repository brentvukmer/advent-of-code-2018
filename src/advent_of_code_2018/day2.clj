(ns advent-of-code-2018.day2
  (:require [clojure.java.io :as io]))

(defn read-data
  [path]
  (with-open [rdr (io/reader (io/resource path))]
    (vec (line-seq rdr))))

;
; 1 or more doubles, increment 2 counter
; 1 or more triples, increment 3 counter
; Multiply 2 counter and 3 counter
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
