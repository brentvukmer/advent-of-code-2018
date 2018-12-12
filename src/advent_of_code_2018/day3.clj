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

(defn parse-input-row
  [s]
  (let [[id left-dist top-dist width height] (map read-string (rest (first (re-seq #"#(\d+) \@ (\d+),(\d+): (\d+)x(\d+)" s))))]
    {:id     id
     :left   left-dist
     :top    top-dist
     :width  width
     :height height}))

(defn read-raw-data
  [path]
  (with-open [rdr (io/reader (io/resource path))]
    (->> (map parse-input-row (line-seq rdr))
         vec)))

(defn convert-row
  "Takes the raw row data as input and returns the x and y for the corners."
  [r]
  (-> r
      (dissoc :left :top :width :height)
      (assoc :x [(:left r) (+ (:width r) (:left r))])
      (assoc :y [(:top r) (+ (:top r) (:height r))])))

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

;
; INTERVAL TREES
;    "Given a set of n intervals on the number line,
;     we want to construct a data structure so that we can efficiently
;     retrieve all intervals overlapping another interval or point."
;
;    See:
;      - https://en.wikipedia.org/wiki/Interval_tree
;      - http://www.dgp.toronto.edu/people/JamesStewart/378notes/22intervals/
;

;
; "Poor man's interval tree"
; Taken from http://clj-me.cgrand.net/2012/03/16/a-poor-mans-interval-tree/
;
(defn interval-lt
  [[a b] [c d]]
  (boolean (and b c
                (if (= a b)
                  (neg? (compare b c))
                  (<= (compare b c) 0)))))

(def empty-interval-map
  (sorted-map-by interval-lt [nil nil] #{}))

(defn- isplit-at [interval-map x]
  (if x
    (let [[[a b :as k] vs] (find interval-map [x x])]
      (if (or (= a x) (= b x))
        interval-map
        (-> interval-map (dissoc k) (assoc [a x] vs [x b] vs))))
    interval-map))

(defn- ialter [interval-map from to f & args]
  (let [interval-map (-> interval-map (isplit-at from) (isplit-at to))
        kvs (for [[r vs]
                  (cond
                    (and from to)
                    (subseq interval-map >= [from from] < [to to])
                    from
                    (subseq interval-map >= [from from])
                    to
                    (subseq interval-map < [to to])
                    :else
                    interval-map)]
              [r (apply f vs args)])]
    (into interval-map kvs)))

(defn iassoc [interval-map from to v]
  (ialter interval-map from to conj v))

(defn idissoc [interval-map from to v]
  (ialter interval-map from to disj v))

(defn iget [interval-map x]
  (get interval-map [x x]))

(defn build-interval-map
  [data k]
  (let [rows (map convert-row data)]
    (reduce (fn [iv-map row]
              (let [vals (k row)]
                (iassoc iv-map (first vals) (inc (last vals)) row)))
            empty-interval-map
            rows)))

(defn axis-range
  [p k]
  (let [c (k p)]
    (apply sorted-set (range (first c) (inc (second c))))))

(defn- x-range
  [p]
  (axis-range p :x))

(defn- y-range
  [p]
  (axis-range p :y))

;
; SWEEP LINE
; Sweep line algorithms are used in solving planar problems.
;   - https://en.wikipedia.org/wiki/Sweep_line_algorithm
;   - http://www.cs.tufts.edu/comp/163/notes05/seg_intersection_handout.pdf
;
; The basic outline of a sweep line algorithm is as follows:
;   - Sweep a line across problem plane.
;   - As the line sweeps across the plane, events of interest occur.
;   - Keep track of these events.
;   - Deal with events that occur at the line leaving a solved problem behind.
;
(defn claim-intersection-points
  [ivmap x]
  (let [x-overlaps (sort-by :x (iget ivmap x))
        maybes
        (if (<= (count x-overlaps) 1)
          '()
          (->> x-overlaps
               (map #(map vector (repeat x) (y-range %)))
               (map #(apply sorted-set %))))]
    (if (<= (count maybes) 1)
      '()
      (->> (combo/combinations maybes 2)
           (map #(apply set/intersection %))
           (filter (comp not empty?))))))

(defn intersect-distances
  ([data start end]
   (let [ivmap (build-interval-map data :x)]
     (for [x (range start (inc end))
           :let [intersections (claim-intersection-points ivmap x) ]
           :when (> (count intersections) 0)]
       {:x x :dist (dec (count intersections))})))
  ([data]
   (intersect-distances data 0 1000)))

;
; Total claim intersection area
;

(defn claim-overlap-area
  [data]
  (->> (intersect-distances data)
       (map :dist)
       (apply +)))

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
  ;........
  ;...2222.
  ;...2222.
  ;.11XX22.
  ;.11XX22.
  ;.111133.
  ;.111133.
  ;........
  (def raw-test-data (read-raw-data "day3-test"))
  (def test-ivmap (build-interval-map raw-test-data :x))
  (def test-intersects (intersect-points raw-test-data 0 11))

  (def raw-data (read-raw-data "day3"))
  (def ivmap (build-interval-map raw-data :x))
  (def ips (intersect-points raw-data))
  (def xys (map #(assoc (dissoc % :intersections) :ys (map second (:intersections %))) ips))

  ;
  ; Draw the claims using Quil to visualize how claims overlap.
  ;
  (def sorted-test-data (sort-by (juxt :left :top) raw-test-data))
  (q/defsketch test-sketch
               :size [10 10]
               :draw (fn [] (draw-claims-with-line 10 sorted-test-data))
               :setup (fn [] (setup 60))
               :features [:resizable])


  (def sorted-data (sort-by (juxt :left :top) raw-data))
  (def sample (take 1000 sorted-data))
  (q/defsketch day3-sketch
               :size [1100 1100]
               :draw (fn [] (draw-claims-with-line 1100 sorted-data))
               :setup (fn [] (setup 60))
               :features [:resizable]))



