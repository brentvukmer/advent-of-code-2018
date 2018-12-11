;
; https://adventofcode.com/2018/day/3
;
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
    (->> (map convert-row-raw (line-seq rdr))
         vec)))

(defn read-data
  [path]
  (map convert-row (read-raw-data path)))

;
;
; Use a sweep line algorithm along with an interval tree
; to create, extend and close "claim intersection area" rectangles.
;  - https://www.reddit.com/r/compsci/comments/kq0jw/overlapping_rectangles/
;
; Sort the claims first by x and then by y.
; Create an interval tree to store the claims by y-interval.
; Sweep a vertical line across the canvas.
; For each value of x, look up claims by y-interval.
; If the previous y had no intersecting claims and the current y does:
;     - If there is already an existing "claim intersection area" rectangle, extend it
;     - Otherwise start a new "claim intersection area" rectangle
;  If the previous y had intersecting claims and the current y does not:
;     - Close the existing "claim intersection area" rectangle
; NOTE: Does this approach handle the case where a new "claim intersection area" rectangle starts due a different y-interval (smaller/bigger)?
;
; SWEEP LINE
; Sweep line algorithms are used in solving planar problems.
;  - https://en.wikipedia.org/wiki/Sweep_line_algorithm
;
; The basic outline of a sweep line algorithm is as follows:
;     - Sweep a line across problem plane.
;     - As the line sweeps across the plane, events of interest occur.
;     - Keep track of these events.
;     - Deal with events that occur at the line leaving a solved problem behind.
;
; INTERVAL TREES
;    "Given a set of n intervals on the number line,
;     we want to construct a data structure so that we can efficiently
;     retrieve all intervals overlapping another interval or point."
;
;    See:
;       - http://www.cs.tufts.edu/comp/163/notes05/seg_intersection_handout.pdf
;       - https://en.wikipedia.org/wiki/Interval_tree
;      - http://www.dgp.toronto.edu/people/JamesStewart/378notes/22intervals/
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
    ; Make the line red by default
   (draw-sweep-line size [255 0 0] (mod (q/frame-count) size))))

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
    (draw-claims data)
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
  (def sorted-test-data (sort-by (juxt :left :top) raw-test-data))
  (q/defsketch test-sketch
               :size [10 10]
               :draw (fn [] (draw-claims-with-line 10 sorted-test-data))
               :setup (fn [] (setup 60))
               :features [:resizable])

  (def raw-data (read-raw-data "day3"))
  (def sorted-data (sort-by (juxt :left :top) raw-data))
  (def sample (take 1000 sorted-data))
  (q/defsketch day3-sketch
               :size [1100 1100]
               :draw (fn [] (draw-claims-with-line 1100 sorted-data))
               :setup (fn [] (setup 60))
               :features [:resizable])

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