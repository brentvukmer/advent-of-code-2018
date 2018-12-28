(ns advent-of-code-2018.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set])
  (:import (java.util GregorianCalendar)))

;--- Day 7: The Sum of Its Parts ---
;You find yourself standing on a snow-covered coastline; apparently, you landed a little off course.
;The region is too hilly to see the North Pole from here, but you do spot some Elves that seem to be
;trying to unpack something that washed ashore. It's quite cold out, so you decide to risk creating
; a paradox by asking them for directions.
;
;"Oh, are you the search party?" Somehow, you can understand whatever Elves from the year 1018 speak;
;you assume it's Ancient Nordic Elvish. Could the device on your wrist also be a translator?
;"Those clothes don't look very warm; take this." They hand you a heavy coat.
;
;"We do need to find our way back to the North Pole, but we have higher priorities at the moment.
;You see, believe it or not, this box contains something that will solve all of Santa's transportation problems -
;at least, that's what it looks like from the pictures in the instructions." It doesn't seem like they can read
;whatever language it's in, but you can: "Sleigh kit. Some assembly required."
;
;"'Sleigh'? What a wonderful name! You must help us assemble this 'sleigh' at once!" They start excitedly pulling more parts out of the box.
;
;The instructions specify a series of steps and requirements about which steps must be finished before others can begin (your puzzle input).
;Each step is designated by a single letter. For example, suppose you have the following instructions:
;
;Step C must be finished before step A can begin.
;Step C must be finished before step F can begin.
;Step A must be finished before step B can begin.
;Step A must be finished before step D can begin.
;Step B must be finished before step E can begin.
;Step D must be finished before step E can begin.
;Step F must be finished before step E can begin.
;
;Visually, these requirements look like this:
;
;
;-->A--->B--
;/    \      \
;C      -->D----->E
;\           /
;---->F-----
;
;Your first goal is to determine the order in which the steps should be completed.
;If more than one step is ready, choose the step which is first alphabetically.
;
;In this example, the steps would be completed as follows:
;
;Only C is available, and so it is done first.
;Next, both A and F are available. A is first alphabetically, so it is done next.
;Then, even though F was available earlier, steps B and D are now also available,
;and B is the first alphabetically of the three.
;After that, only D and F are available. E is not available because only some
;of its prerequisites are complete. Therefore, D is completed next.
;F is the only choice, so it is done next.
;Finally, E is completed.
;
;So, in this example, the correct order is CABDFE.
;
;In what order should the steps in your instructions be completed?

(defn parse-input-row
  [r]
  (->> (re-matches #"Step ([A-Z]+) must be finished before step ([A-Z]+) can begin." r)
       rest
       (map keyword)
       vec))

(defn read-raw-data
  [path]
  (with-open [rdr (io/reader (io/resource path))]
    (mapv parse-input-row (line-seq rdr))))

(defn links
  [data from-fn to-fn]
  (->> data
       (map #(list (from-fn %) (to-fn %)))
       (group-by first)
       (map #(vector (first %) (map second (second %))))
       (into {})))

(defn collect-steps
  [accumulator evaluation-set forward-links back-links]
  (println "accumulator: " accumulator)
  (println "evaluation-set: " evaluation-set)
  (println "forward-links: " forward-links)
  (println "back-links: " back-links)
  (let [next-step (->> (filter #(let [requirements (set (get back-links %))]
                                  (or (nil? requirements)
                                      (= requirements
                                         (set/intersection requirements (set accumulator))))) evaluation-set)
                       sort
                       first)]
    (println "next-step: " next-step)
    (if next-step
      (collect-steps (conj accumulator next-step)
                     (set/union (disj evaluation-set next-step)
                                (apply sorted-set (get forward-links next-step)))
                     forward-links
                     back-links)
      accumulator)))

(defn print-steps
  [data]
  (let [back-links (links data second first)
        forward-links (links data first second)
        start (first (set/difference (set (keys forward-links)) (set (keys back-links))))]
    ; Create a vector to accumulate steps
    ; Get current step (first in alphabetical order from evaluation set)
    ; If the current step is satisfied:
    ;  - conj it into accumulator
    ;  - add current step's forward links to the evaluation set
    ; Otherwise:
    ;  - add current step to the evaluation set
    ;  - add current step's forward links to the evaluation set
    (->> (collect-steps [] #{start} forward-links back-links)
         (map name)
         (apply str))))

