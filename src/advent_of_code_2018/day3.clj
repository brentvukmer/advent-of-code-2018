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

;
; Dirty, dirty logic for identifying "events of interest"
;
(defn adjacent?
  [rects]
  (let [x-sorted (sort-by :x rects)]
    (or (= (first (:x (last x-sorted))) (last (:x (first x-sorted))))
        (= (first (:y (last x-sorted))) (last (:y (first x-sorted)))))))

(defn claim-intersection-points
  [ivmap x]
  (let [x-overlaps (->> (iget ivmap x)
                        (sort-by :x))
        maybes
        (if (< (count x-overlaps) 2)
          '()
          (->> (flatten (filter (comp not adjacent?) (combo/combinations x-overlaps 2)))
               (map #(map vector (repeat x) (y-range %)))
               (map #(apply sorted-set %))))]
    (if (< (count maybes) 2)
      '()
      (->> (combo/combinations maybes 2)
           (map #(apply set/intersection %))
           (filter not-empty)
           first))))

;
; Sweep algorithm
;
(defn intersect-points
  ([data start end]
   (let [ivmap (build-interval-map data :x)]
     (for [x (range start (inc end))
           :let [points (claim-intersection-points ivmap x)]
           :when (not-empty points)]
       {:x      x
        :points points})))
  ([data]
   (intersect-points data 0 1000)))

;
;TODO: Aggregate points into segments; aggregate segments into rectangles
;

;
; TODO: Group points into rectangles, then calculate area per rectangle, then sum
;
(defn area
  [top-left bottom-right]
  (* (- (first bottom-right) (first top-left))
     (- (second bottom-right) (second top-left))))




