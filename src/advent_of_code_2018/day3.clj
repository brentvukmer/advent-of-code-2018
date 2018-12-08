(ns advent-of-code-2018.day3
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [quil.core :as q]))

;
; PART 1
;

; All claims have an ID and consist of a single rectangle with edges parallel to the edges of the fabric. Each claim's rectangle is defined as follows:
;
; The number of inches between the left edge of the fabric and the left edge of the rectangle.
; The number of inches between the top edge of the fabric and the top edge of the rectangle.
; The width of the rectangle in inches.
; The height of the rectangle in inches.

(defn convert-row-raw
  [s]
  (let [[id left-dist top-dist width height] (map read-string (rest (first (re-seq #"#(\d+) \@ (\d+),(\d+): (\d+)x(\d+)" s))))]
    {:id     id
     :left   left-dist
     :top    top-dist
     :width  width
     :height height}))

(defn convert-row
  "Takes the raw row data as input and returns the x and y for the corners."
  [r]
  (-> r
      (dissoc :left :top :width :height)
      (assoc :x [(:left r) (+ (:width r) (:left r))])
      (assoc :y [(:top r) (+ (:top r) (:height r))])))

(defn read-raw-data
  [path]
  (with-open [rdr (io/reader (io/resource path))]
    (vec (map convert-row-raw (line-seq rdr)))))

(defn read-data
  [path]
  (map convert-row (read-raw-data path)))

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

(defn- k-adj?
  [i k]
  (->> (k i)
       set
       count
       (= 1)))

(defn- adjacent-claims?
  [i]
  (or
    (k-adj? i :x)
    (k-adj? i :y)))

(defn- intersection
  [p1 p2]
  (let [x-intersect (set/intersection (x-range p1) (x-range p2))
        y-intersect (set/intersection (y-range p1) (y-range p2))]
    {:claims (if (:claims p1)
               [(first (:claims p1)) (first (:claims p2))]
               [[(:id p1) (:id p2)]])
     :x      [(first x-intersect) (last x-intersect)]
     :y      [(first y-intersect) (last y-intersect)]}))


(defn intersections
  [data]
  (->> (combo/combinations data 2)
       (map #(intersection (first %) (second %)))
       (remove adjacent-claims?)
       set
       vec))

(defn area
  [overlap]
  (->> (apply - (:x overlap))
       (* (apply - (:y overlap)))
       Math/abs))

(defn overlaps-area
  [data]
  (let [overlaps (intersections data)
        overlaps2 (intersections overlaps)]
    (- (apply + (map area overlaps))
       (apply + (map area overlaps2)))))

(defn quil-input
  [r s]
  [(:left r) (:top r) (* s (:width r)) (* s (:height r))])

(defn draw-claims
  ([claims scale]
   (q/background 255)
   (doseq [c (map #(quil-input % scale) claims)]
     (apply q/rect c)))
  ([claims]
    (draw-claims claims 1)))

;
; REPL time-savers
;
(comment
  (def test-data (read-data "day3-test"))
  (def p1 (first test-data))
  (def p2 (second test-data))
  (def p3 (nth test-data 2))
  (def test-area (overlaps-area test-data))

  (def data (read-data "day3"))
  (def overlaps (intersections data))

  ;
  ; Draw the claims using Quil to visualize how claims overlap.
  ;

  (q/defsketch test-sketch
               :size [10 10]
               :draw (fn [] (draw-claims (read-raw-data "day3-test") 10))
               :title "Day 3 Test Data"
               :features [:keep-on-top :resizable])

  (q/defsketch day3-sketch
               :size [1000 1000]
               :draw (fn [] (draw-claims (read-raw-data "day3")))
               :title "Day 3 Data"
               :features [:keep-on-top :resizable])


  (def overlaps2 (intersections overlaps))
  (def grouped-overlaps (->> (conj
                               (group-by #(first (:claims %)) overlaps)
                               (group-by #(second (:claims %)) overlaps))
                             (into (sorted-map))))
  (def grouped-overlaps2 (->> overlaps2
                              (group-by #(dissoc % :claims))
                              (map #(vector (mapv :claims (second %)) (first %)))
                              (into {})))
  (def part1 (overlaps-area data)))