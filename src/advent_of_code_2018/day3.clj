;
; https://adventofcode.com/2018/day/3
;
(ns advent-of-code-2018.day3
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [quil.core :as q]))

;
; PART 1
;
; PROBLEM STATEMENT

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
; How many square inches of fabric are within two or more claims?
;
; DATA NOTES
;
; The input data is expressed in terms of top-left grid coordinates for claim rectangles.
; Howver the fundamental unit for measuring claim area (including intersecting claim areas)
; is a 1x1 square. So after parsing the input rows into rectangles using top-left grid coordinates,
; we need to create a mapping of x values to 1x1 squares whose left edge is x.
;
; PROPOSED SOLUTION
;
; Use a sweep line algorithm along with an interval tree to flag 1x1 squares
; on the grid that belong to at least two claims.
;
; See https://www.reddit.com/r/compsci/comments/kq0jw/overlapping_rectangles/
;
; Parse the input to get the claim rectangle coordinates,
; Create an interval tree to store the claims by x-interval.
; Create a map of 1x1 square top left corner coordinates -> claim ids.
; Create a function that selects all keys from that map which match x.
;
; Sweep a vertical line across the canvas.
; For each value of x:
;    - Get the 1x1 squares whose left edge is on x.
;    - Look up claims by x-interval.
;    - For each claim:
;       - Get the y range.
;    -  - Find the squares whose max/min y values are in the y-range
;       - Update the map entry for each matching square with the claim id
;    - Select the 1x1 squares that map to 2 or more claims
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
     (mapv parse-input-row (line-seq rdr))))

(defn convert-row
  "Takes the raw row data as input and returns the x and y for the corners."
  [r]
  (-> r
      (dissoc :left :top :width :height)
      (assoc :x [(:left r) (+ (:width r) (:left r))])
      (assoc :y [(:top r) (+ (:top r) (:height r))])))

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
(defn initial-state
  [data size]
  (let [ivmap (build-interval-map data :x)]
    {:full-data data
     :ivmap  ivmap
     :size size
     :max-xy (->> (vals ivmap)
                  (apply set/union)
                  (remove empty?)
                  (map #(vector (second (:x %)) (second (:y %))))
                  sort
                  last)}))

(defn unit-square
  [[x y]]
  {:x [x (inc x)] :y [y (inc y)]})

(defn contained-by?
  [unit-square rectangle]
  (and
    (>= (first (:x unit-square)) (first (:x rectangle)))
    (<= (second (:x unit-square)) (second (:x rectangle)))
    (>= (first (:y unit-square)) (first (:y rectangle)))
    (<= (second (:y unit-square)) (second (:y rectangle)))))

(defn unit-square-claims
  [init x]
  (let [max-y (second (:size init))
        y-range (range 0 (inc max-y))
        unit-squares (map #(unit-square (vector x %)) y-range)
        x-overlaps (->> (iget (:ivmap init) x)
                        (sort-by :x))]
    (map #(hash-map :unit-square %
                    :claims (->> (filter (fn [r] (contained-by? % r)) x-overlaps)
                                 (map :id)
                                 set)) unit-squares)))

(defn filter-contested
  [squares]
  (filter #(>= (count (:claims %)) 2) squares))

(defn contested-unit-squares
  [state]
  (->>
    (for [x (range 0 (inc (first (:size state))))]
      (->> (unit-square-claims state x)
           filter-contested))
    (apply concat)))

(comment
  (def contested (contested-unit-squares (initial-state (read-raw-data "day3") [1100 1100])))
  )

;
; PART 2
;

(defn uncontested-claims
  [data contested-squares]
  (let [all-claims (apply sorted-set (map :id data))
        overlapping-claims (apply set/union (map :claims contested-squares))]
    (set/difference all-claims overlapping-claims)))



