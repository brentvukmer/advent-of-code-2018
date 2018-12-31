(ns advent-of-code-2018.day6
  (:require [clojure.java.io :as io]))

;
;Using only the Manhattan distance, determine the area around each coordinate
;by counting the number of integer X,Y locations that are closest to that coordinate
;(and aren't tied in distance to any other coordinate).
;
;Your goal is to find the size of the largest area that isn't infinite.
;
;For example, consider the following list of coordinates:
;
;1, 1
;1, 6
;8, 3
;3, 4
;5, 5
;8, 9
;
;If we name these coordinates A through F, we can draw them on a grid, putting 0,0 at the top left:
;
;..........
;.A........
;..........
;........C.
;...D......
;.....E....
;.B........
;..........
;..........
;........F.
;
;This view is partial - the actual grid extends infinitely in all directions.
;Using the Manhattan distance, each location's closest coordinate can be determined,
;shown here in lowercase:
;
;aaaaa.cccc
;aAaaa.cccc
;aaaddecccc
;aadddeccCc
;..dDdeeccc
;bb.deEeecc
;bBb.eeee..
;bbb.eeefff
;bbb.eeffff
;bbb.ffffFf
;
;Locations shown as . are equally far from two or more coordinates,
;and so they don't count as being closest to any.
;
;In this example, the areas of coordinates A, B, C, and F are infinite -
;while not shown here, their areas extend forever outside the visible grid.
;However, the areas of coordinates D and E are finite: D is closest to 9 locations,
;and E is closest to 17 (both including the coordinate's location itself).
;Therefore, in this example, the size of the largest area is 17.
;
;What is the size of the largest area that isn't infinite?

(defn read-raw-data
  [path]
  (with-open [rdr (io/reader (io/resource path))]
    (mapv #(read-string (str "[" % "]")) (line-seq rdr))))

(defn manhattan-distance
  ""
  ([position]
   (+ (Math/abs ^Integer (first position)) (Math/abs ^Integer (second position))))
  ([pos2 pos1]
   (manhattan-distance (map - pos2 pos1))))

; Use a vector to represent the grid (2-D flattened into 1D).
; Construct the grid starting at top-left as 0,0 (to max x + 1, max y).
; Sort the designated locations (first by x, then y) and put into a vector.
; At each designated location, put the id (index) for that location, at that location.
; At each location in the grid that is NOT a designated location, start with the ids for all designated locations.
;   - Find closest designated locations
;   - If they are equidistant from the current location, put nil
;   - If one is closer, put that one's id

(defn neighbor-dist
  [loc others]
  (->> (map #(vector % (manhattan-distance loc %)) others)
       (into {})))

(defn fill-grid
  [data size]
  (let [sorted (vec (sort-by (juxt first second) data))]
    (for [x (range 0 size)
          y (range 0 size)
          :let [loc [x y]
                closest-dists (->> (neighbor-dist loc sorted)
                                   (sort-by second)
                                   (take 2))
                closest (if (= (second (first closest-dists))
                               (second (second closest-dists)))
                          nil
                          (first closest-dists))]]
      [loc (first closest)])))

(defn infinite-area?
  [size area]
  (let [xs (->> (second area)
                (map first)
                set)
        ys (->> (second area)
                (map second)
                set)]
    (or (some #{0 (dec size)} xs)
        (some #{0 (dec size)} ys))))

(defn grid-finite-areas
  [data]
  (let [size (inc (max (apply max (map first data))
                       (apply max (map second data))))]
    (->> (fill-grid data size)
         (group-by #(second %))
         (map #(vector (first %) (mapv first (second %))))
         (remove #(infinite-area? size %)))))

(defn grid-max-finite-area
  [data]
  (->> (grid-finite-areas data)
       (map #(vector (first %) (count (second %))))
       (sort-by second)
       last))

;--- Part Two ---
;On the other hand, if the coordinates are safe, maybe the best you can do
;is try to find a region near as many coordinates as possible.
;
;For example, suppose you want the sum of the Manhattan distance to
;all of the coordinates to be less than 32. For each location, add up the
;distances to all of the given coordinates; if the total of those distances
;is less than 32, that location is within the desired region.
;
;Using the same coordinates as above, the resulting region looks like this:
;
;..........
;.A........
;..........
;...###..C.
;..#D###...
;..###E#...
;.B.###....
;..........
;..........
;........F.
;
;In particular, consider the highlighted location 4,3 located at the
;top middle of the region. Its calculation is as follows, where
;abs() is the absolute value function:
;
;Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30
;
;Because the total distance to all coordinates (30) is less than 32,
;the location is within the region.
;
;This region, which also includes coordinates D and E, has a total size of 16.
;
;Your actual region will need to be much larger than this example, though,
; instead including all locations with a total distance of less than 10000.
;
;What is the size of the region containing all locations which have a total
; distance to all given coordinates of less than 10000?

(defn fill-grid-total-dist
  [data size]
  (let [sorted (vec (sort-by (juxt first second) data))]
    (for [x (range 0 size)
          y (range 0 size)
          :let [loc [x y]
                dists (neighbor-dist loc sorted)]]
      [loc (apply + (map second dists))])))

(defn filter-grid-total-dist
  [data limit]
  (let [size (inc (max (apply max (map first data))
                       (apply max (map second data))))]
    (->> (fill-grid-total-dist data size)
         (remove #(>= (second %) limit)))))

(defn total-dist-region-area
  [data limit]
  (count (filter-grid-total-dist data limit)))