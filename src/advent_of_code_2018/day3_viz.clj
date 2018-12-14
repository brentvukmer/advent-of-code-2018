(ns advent-of-code-2018.day3-viz
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [advent-of-code-2018.day3 :as day3]))

;
; Quil functions for visualizing claims
;

; 'setup' is a cousin of 'draw' function
; setup initialises sketch and it is called only once
; before draw called for the first time
(defn setup
  [{:keys [target-frame-rate full-data selected-data intersections size corners] :or {target-frame-rate 60 full-data [] selected-data [] intersections {} size [1100 1100] corners [] }}]
  ; draw will be called at this rate (per second)
  (q/frame-rate target-frame-rate)
  ; set background to white colour only in the setup
  ; otherwise each invocation of 'draw' would clear sketch completely
  (q/background 255)
  {:target-frame-rate target-frame-rate
   :full-data full-data
   :selected-data selected-data})

(defn draw-claims
  [claims]
  ; Make the line black
  (q/stroke 0 0 0)
  (doseq [c claims]
    (let [left (:left c)
          top (:top c)
          width (:width c)
          height (:height c)]
      ; q/line instead of q/rect so that intersections don't get covered
      (q/line [left top] [(+ width left) top])
      (q/line [(+ width left) top] [(+ width left) (+ height top)])
      (q/line [(+ width left) (+ height top)] [left (+ height top)])
      (q/line [left (+ height top)] [left top]))))

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
  [len x]
  (q/stroke 255 255 255)
  (q/line [len 0] [len len])
  (doseq [x- (range 0 x)]
    (q/line [x- 0] [x- len])))

(defn draw-claims-with-line
  [{:keys [size full-data selected-data intersections]}]
  (let [len (first size)
        x (mod (q/frame-count) len)
        data (if selected-data selected-data full-data)]
    (do
      (clear-sweep-lines len x)
      (draw-claims data)
      (doseq [t (range 0 x)]
        (draw-points (get intersections t)))
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
; Capture and test the simpler, smaller clusters and get them working, one at a time.
; Then capture and test some of the larger clusters and get them working.
; Finally, capture and test some of the most complex clusters and get them working.
;

;
; REPL time-savers
;
;........
;...2222.
;...2222.
;.11XX22.
;.11XX22.
;.111133.
;.111133.
;........
(def raw-test-data (day3/read-raw-data "day3-test"))
(def test-ivmap (day3/build-interval-map raw-test-data :x))
(def test-ips (day3/intersect-points raw-test-data))
(def test-x-points (into {} (map #(vector (:x %) (:points %)) test-ips)))
(def sorted-test-data (sort-by (juxt :left :top) raw-test-data))

(def raw-data (day3/read-raw-data "day3"))
(def prod-ivmap (day3/build-interval-map raw-data :x))
(def ips (day3/intersect-points raw-data))
(def x-points (into {} (map #(vector (:x %) (:points %)) ips)))
(def sorted-data (sort-by (juxt :left :top) raw-data))

;
; Draw the claims using Quil to visualize how claims overlap.
;
(def test-size [10 10])
(q/defsketch test-sketch
             :size test-size
             :draw draw-claims-with-line
             :setup (fn [] (setup {:target-frame-rate 60
                                   ::full-data        sorted-test-data
                                   :intersections     test-x-points
                                   :size              test-size}))
             :features [:resizable]
             :middleware [m/fun-mode])

(q/defsketch day3-sketch
             :size prod-size
             :draw draw-claims-with-line
             :setup (fn [] (setup {:full-data     sorted-data
                                   :intersections x-points}))
             :features [:resizable]
             ;:middleware [m/fun-mode]
             )
