(ns advent-of-code-2018.day3
  (:require [clojure.java.io :as io]))


;The Elves managed to locate the chimney-squeeze prototype fabric for Santa's suit
; (thanks to someone who helpfully wrote its box IDs on the wall of the warehouse
; in the middle of the night). Unfortunately, anomalies are still affecting them - nobody can even agree on how to cut the fabric.
;
;The whole piece of fabric they're working on is a very large square - at least 1000 inches on each side.
;
;Each Elf has made a claim about which area of fabric would be ideal for Santa's suit.
; All claims have an ID and consist of a single rectangle with edges parallel to the edges of the fabric. Each claim's rectangle is defined as follows:
;
;The number of inches between the left edge of the fabric and the left edge of the rectangle.
;The number of inches between the top edge of the fabric and the top edge of the rectangle.
;The width of the rectangle in inches.
;The height of the rectangle in inches.

(defn convert-row
  [s]
  (let [[id left-dist top-dist width height] (map read-string (rest (first (re-seq #"#(\d+) \@ (\d+),(\d+): (\d+)x(\d+)" s))))]
    {:id id :dists {:left left-dist :top top-dist} :dimensions {:width width :height height}}))

(defn read-data
  [path]
  (with-open [rdr (io/reader (io/resource path))]
    (mapv convert-row (line-seq rdr))))
