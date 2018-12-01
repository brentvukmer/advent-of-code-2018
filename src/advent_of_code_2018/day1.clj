(ns advent-of-code-2018.day1
  (:require [clojure.java.io :as io]))

(defn read-data
  [path]
  (read-string (str "[" (slurp (io/resource path)) "]")))

(defn calibrate
  []
  (apply + (read-data "day1")))

;
; 0. Create atom (set) to store already seen values
; 1. Run reductions on the input with last reduction as the initial value (zero to start).
; 2. Filter intermediate values found in the atom.
; 3. Return first matching intermediate value, if any are found.
; 4. Otherwise, store all intermediate values in atom, and repeat steps 1-4
;

;
; Use:
; - let (create atom)
; - take-while
; - iterate
; - reductions
;

(defn first-dup
  []
  (let [memo (atom #{})
        inputs (read-data "day1")]
    (loop [sum-val 0
           tracker @memo]
      (let [freqs (rest (reductions + sum-val inputs))
            match (some @memo freqs)]
        (or
          match
          (recur (last freqs)
                 (swap! memo clojure.set/union (set freqs))))))))