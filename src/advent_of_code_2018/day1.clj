(ns advent-of-code-2018.day1
  (:require [clojure.java.io :as io]))

(defn read-data
  [path]
  (read-string (str "[" (slurp (io/resource path)) "]")))

(defn calibrate
  []
  (apply + (read-data "day1")))

;
; 1. Run reductions on the input with 0 as the initial value.
; 2. Save the intermediate values.
; 3. Use the last reduction as the new initial value, and reduce input again.
; 4. Concat the new intermediate values.
; 5. Stop if a dup is found in the intermediate values.
; 6. Otherwise, repeat steps 3-6
;

(defn dup-freqs
  []
  (let [freqs (reductions + (read-data "day1"))]
    freqs))