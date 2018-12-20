(ns advent-of-code-2018.day1
  (:require [clojure.java.io :as io]))

(defn read-data
  [path]
  (read-string (str "[" (slurp (io/resource path)) "]")))

;
; PART 1
;
(defn calibrate
  []
  (apply + (read-data "day1")))

;
; PART 2
;
; 0. Create set to store already seen values
; 1. Run reductions on the input with last reduction as the initial value (zero to start).
; 3. Return first matching intermediate value, if any are found.
; 4. Otherwise, store all intermediate values in atom, and repeat steps 1-4
;
(defn first-dup
  []
  (let [inputs (read-data "day1")]
    (loop [sum-val 0
           memo #{}]
      (let [freqs (rest (reductions + sum-val inputs))]
        (or
          (some memo freqs)
          (recur (last freqs)
                 (clojure.set/union memo (set freqs))))))))