(ns advent-of-code-2018.day3
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

;
; PART 1
;

; All claims have an ID and consist of a single rectangle with edges parallel to the edges of the fabric. Each claim's rectangle is defined as follows:
;
; The number of inches between the left edge of the fabric and the left edge of the rectangle.
; The number of inches between the top edge of the fabric and the top edge of the rectangle.
; The width of the rectangle in inches.
; The height of the rectangle in inches.

(defn convert-row
  [s]
  (let [[id left-dist top-dist width height] (map read-string (rest (first (re-seq #"#(\d+) \@ (\d+),(\d+): (\d+)x(\d+)" s))))]
    {:id id
     :x  [left-dist (+ width left-dist)]
     :y  [top-dist (+ height top-dist)]}))

(defn read-data
  [path]
  (with-open [rdr (io/reader (io/resource path))]
    (vec (map convert-row (line-seq rdr)))))

;
; How many square inches of fabric are within two or more claims?
;
; - Convert dist + dimensions into graph coordinates
; - Generate all combinations of rectangles.
; - Identify overlapping rectangles.
; - Calculate area for each overlap.
; - Solve for more than
; - Sum the total area covered by overlaps.
;

(defn- axis-range
  [p k]
  (let [c (k p)]
    (apply sorted-set (range (first c) (inc (second c))))))

(defn- x-range
  [p]
  (axis-range p :x))

(defn- y-range
  [p]
  (axis-range p :y))

(defn- adjacent-claims?
  [i]
  (or
    (= 1 (count (set (:x i))))
    (= 1 (count (set (:y i))))))

(defn- intersection
  [p1 p2]
  (let [x-intersect (set/intersection (x-range p1) (x-range p2))
        y-intersect (set/intersection (y-range p1) (y-range p2))
        x-overlap [(first x-intersect) (last x-intersect)]
        y-overlap [(first y-intersect) (last y-intersect)]
        result {:claims [(:id p1) (:id p2)]
                :x      [(first x-overlap) (last x-overlap)]
                :y      [(first y-overlap) (last y-overlap)]}]
    (if (adjacent-claims? result)
      nil
      result)))

(defn overlap-points
  [data]
  (->> (combo/combinations data 2)
       (map #(intersection (first %) (second %)))
       (remove nil?)
       (map #(for [x (x-range %)
                   y (y-range %)]
               [x y]))
       first
       set))

(defn part1
  [data]
  (count (overlap-points data)))

(comment
  (def test-data (read-data "day3-test"))
  (def p1 (first test-data))
  (def p2 (second test-data))
  (def p3 (nth test-data 2))
  (def data (read-data "day3"))
  (def points (overlap-points data)))