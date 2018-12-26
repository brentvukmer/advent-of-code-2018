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
  )

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


