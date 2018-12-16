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
  [state]
  ; draw will be called at this rate (per second)
  (q/frame-rate (:target-frame-rate state))
  ; set background to white colour only in the setup
  ; otherwise each invocation of 'draw' would clear sketch completely
  (q/background 255)
  (-> (day3/read-raw-data (:data-path state))
       (day3/initial-state (:size state))
       (conj (assoc state :num-sweeps 0))))

(defn update-state
  [state]
  (assoc state :x-val (mod (q/frame-count) (first (:size state)))
               :num-sweeps (inc (:num-sweeps state))))

(defn mouse-dragged
  [state event]
  (let [selection [[(:p-x event) (:p-y event)] [(:x event) (:y event)]]]
    (println "mouse-dragged selection " selection)
    (assoc state :selection selection)))

(defn mouse-clicked
  [state event]
  (assoc state :selection [[0 0] [0 0]]))

(defn draw-claims
  [claims]
  ; Make the line black
  (q/stroke 0 0 0)
  (doseq [c claims]
    ; q/line instead of q/rect so that intersections don't get covered
    (q/line [(first (:x c)) (first (:y c))] [(second (:x c)) (first (:y c))])
    (q/line [(second (:x c)) (first (:y c))] [(second (:x c)) (second (:y c))])
    (q/line [(second (:x c)) (second (:y c))] [(first (:x c)) (second (:y c))])
    (q/line [(first (:x c)) (second (:y c))] [(first (:x c)) (first (:y c))])))

(defn draw-squares
  [squares]
  (q/stroke 255 0 0 120)
  (if (seq squares)
    (doseq [s squares]
      (let [{{:keys [x y]} :unit-square} s]
        (q/rect (first x) (first y) (apply - (reverse x)) (apply - (reverse y)))))))

(defn draw-claims-with-line
  [state]
  ; Don't keep sweeping once we've gone across the canvas once
  (if (<= (:num-sweeps state) (first (:size state)))
    (let [x (:x-val state)
          claims (day3/iget (:ivmap state) x)
          squares (->> (day3/unit-square-claims state x)
                       day3/filter-contested)]
      (if (seq claims)
        (do
          (draw-claims claims)
          (draw-squares squares))))))


;
; Quil text display: http://quil.info/api/typography/loading-and-displaying#text
;

(comment
  ;
  ; Draw the claims using Quil to visualize how claims overlap.
  ;
  (def test-size [10 10])
  (q/defsketch test-sketch
               :size test-size
               :update update-state
               :draw draw-claims-with-line
               :setup (fn [] (setup {:target-frame-rate 60
                                     :data-path         "day3-test"
                                     :size              test-size}))
               :features [:resizable]
               :middleware [m/fun-mode]
               :mouse-dragged mouse-dragged
               :mouse-clicked mouse-clicked)

  (def prod-size [1100 1100])
  (q/defsketch day3-sketch
               :size prod-size
               :update update-state
               :draw draw-claims-with-line
               :setup (fn [] (setup {:target-frame-rate 60
                                     :data-path         "day3"
                                     :size              prod-size}))
               :features [:resizable]
               :middleware [m/fun-mode]
               :mouse-dragged mouse-dragged
               :mouse-clicked mouse-clicked)
  )
