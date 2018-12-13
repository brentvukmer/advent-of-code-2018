(ns advent-of-code-2018.day3-viz
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [advent-of-code-2018.day3 :as day3]))

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

(defn draw-points
  [points]
  (q/stroke 0 0 255 100)
  (doseq [p points]
    (apply q/point p)))

(defn clear-sweep-lines
  [size x]
  (q/stroke 255 255 255)
  (q/line [size 0] [size size])
  (doseq [x- (range 0 x)]
    (q/line [x- 0] [x- size])))

(defn draw-claims-with-line
  [size data points]
  (let [x (mod (q/frame-count) size)]
    (do
      (clear-sweep-lines size x)
      (draw-claims data)
      (doseq [t (range 0 x)]
        (draw-points (get points t)))
      (draw-sweep-line size))))

;
; TODO: Confirm that we are using the same coordinate system internally as what Quil expects
;

;
; TODO: Use Quil fun mode to update text box in upper righthand corner with top-left and bottom-right corners
;
; Quil fun mode: https://github.com/quil/quil/wiki/Functional-mode-%28fun-mode%29
; Quil text display: http://quil.info/api/typography/loading-and-displaying#text
;
; --- Display coordinates for selection area ---
; Use mouse-dragged to capture the corner coordinates (top-left and bottom-right) of the area of interest on the canvas.
; Use mouse-clicked to set the :top-left and :bottom-right coordinates to [0, 0].
; Create an update function that updates state with the :top-left and :bottom-right coordinates.
; In setup, create a text box in the upper right-hand corner.
; Make the draw function take a state param; if an area is selected, update the text box with the current values for
; :top-left and :bottom-right coordinates.
;
; --- Draw only rectangles contained in the selection area ---
; Store :full-data in state as part of setup.
; Modify the update function to set :selected-data in state (filter :full-data using :top-left and :bottom-right; if at [0,0], set :selected-data to empty list).
; If :top-left and :bottom-right are not at [0, 0], then draw rectangles using :selected-data.
; Otherwise, draw rectangles using :full-data.
;
; --- Test intersection-points against just 1 cluster of intersecting claims ---
; Iterate over some of the simpler ones, before trying to capture and test some of the larger clusters.


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
  (def test-ips (intersect-points raw-test-data))
  (def test-x-points (into {} (map #(vector (:x %) (:points %)) test-ips)))
  (def sorted-test-data (sort-by (juxt :left :top) raw-test-data))

  (def raw-data (read-raw-data "day3"))
  (def prod-ivmap (build-interval-map raw-data :x))
  (def ips (intersect-points raw-data))
  (def x-points (into {} (map #(vector (:x %) (:points %)) ips)))
  (def sorted-data (sort-by (juxt :left :top) raw-data))


  ;
  ; Draw the claims using Quil to visualize how claims overlap.
  ;
  (q/defsketch test-sketch
               :size [10 10]
               :draw (fn [] (draw-claims-with-line 10 sorted-test-data test-x-points))
               :setup (fn [] (setup 60))
               :features [:resizable])


  (q/defsketch day3-sketch
               :size [1100 1100]
               :draw (fn [] (draw-claims-with-line 1100 sorted-data x-points))
               :setup (fn [] (setup 60))
               :features [:resizable]))
