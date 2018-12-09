(ns advent-of-code-2018.day3
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [quil.core :as q]))

;
; PART 1
;

; All claims have an ID and consist of a single rectangle
; with edges parallel to the edges of the fabric.
;
; Each claim's rectangle is defined as follows:
;
; - The number of inches between the left edge of the fabric and the left rectangle edge.
; - The number of inches between the top edge of the fabric and the top rectangle edge.
; - The width of the rectangle in inches.
; - The height of the rectangle in inches.
;
;
; How many square inches of fabric are within two or more claims?
;

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

;
; Quil functions for visualizing claims
;
(defn draw-claims
  [claims]
  ; Make the line black
  (q/stroke 0 0 0)
  (doseq [c claims]
    (let [left (:left c)
          top (:top c)
          width (:width c)
          height (:height c)]
      ; Draw each line in the rectangle so that intersections don't get covered
      (q/line [left top] [(+ width left) top])
      (q/line [(+ width left) top] [(+ width left) (+ height top)])
      (q/line [(+ width left) (+ height top)] [left (+ height top)])
      (q/line [left (+ height top)] [left top]))))

; 'setup' is a cousin of 'draw' function
; setup initialises sketch and it is called only once
; before draw called for the first time
(defn setup
  [rate]
  ; draw will be called at this rate (per second)
  (q/frame-rate rate)
  ; set background to white colour only in the setup
  ; otherwise each invocation of 'draw' would clear sketch completely
  (q/background 255))

(defn draw-sweep-line
  ([size color x]
   (apply q/stroke color)
   (q/line [x 0] [x size]))
  ([size]
    ; Make the line semi-transparent red by default
   (draw-sweep-line size [255 0 0 120] (mod (q/frame-count) size))))

(defn clear-sweep-lines
  [size x]
  (q/stroke 255 255 255)
  (q/line [size 0] [size size])
  (doseq [x- (range 0 x)]
    (q/line [x- 0] [x- size])))

(defn draw-claims-with-line
  [size data]
  (do
    (clear-sweep-lines size (mod (q/frame-count) size))
    (draw-claims (sort-by :left data))
    (draw-sweep-line size)))

;
; REPL time-savers
;
(comment
  ;
  ; Read inputs
  ;
  (def test-data (read-data "day3-test"))
  (def p1 (first test-data))
  (def p2 (second test-data))
  (def p3 (nth test-data 2))
  (def test-area (overlaps-area test-data))

  (def data (read-data "day3"))

  ;
  ; Draw the claims using Quil to visualize how claims overlap.
  ;
  (def raw-test-data (read-raw-data "day3-test"))
  (q/defsketch test-sketch
               :size [10 10]
               :draw (fn [] (draw-claims-with-line 10 raw-test-data))
               :setup (fn [] (setup 60))
               :features [:resizable])

  (q/defsketch day3-sketch
               :size [1100 1100]
               :draw (fn [] (draw-claims-with-line 1100 raw-data))
               :setup (fn [] (setup 120))
               :features [:resizable])

  ;
  ; See https://www.reddit.com/r/compsci/comments/kq0jw/overlapping_rectangles/
  ;
  ; Use a sweep line algorithm along with an interval tree
  ; to test for (interval)-(set of intervals) collision in O(log(n))
  ;
  ; Determine total intersection area for each cluster.
  ;


  ;
  ; Find claim-intersection clusters.
  ;
  (def overlaps (intersections data))
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