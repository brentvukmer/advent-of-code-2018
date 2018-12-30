(ns advent-of-code-2018.day7
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

;PART 1
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
  (if (empty? evaluation-set)
    accumulator
    (let [next-step (->> (filter #(let [requirements (set (% back-links))]
                                    (or (nil? requirements)
                                        (= requirements
                                           (set/intersection requirements (set accumulator))))) evaluation-set)
                         sort
                         first)]
      (if next-step
        (collect-steps (conj accumulator next-step)
                       (set/union (disj evaluation-set next-step)
                                  (apply sorted-set (next-step forward-links)))
                       forward-links
                       back-links)
        accumulator))))

(defn part1
  [data]
  (let [back-links (links data second first)
        forward-links (links data first second)
        start-set (apply sorted-set (set/difference (set (keys forward-links)) (set (keys back-links))))]
    (->> (collect-steps [] start-set forward-links back-links)
         (map name)
         (apply str))))

;PART 2
;
;As you're about to begin construction, four of the Elves offer to help.
;"The sun will set soon; it'll go faster if we work together."
;
;Now, you need to account for multiple people working on steps simultaneously.
;If multiple steps are available, workers should still begin them in alphabetical order.
;
;Each step takes 60 seconds plus an amount corresponding to its letter: A=1, B=2, C=3, and so on.
;So, step A takes 60+1=61 seconds, while step Z takes 60+26=86 seconds.
;No time is required between steps.
;
;To simplify things for the example, however, suppose you only have help from one Elf
;(a total of two workers) and that each step takes 60 fewer seconds
;(so that step A takes 1 second and step Z takes 26 seconds).
;
;Then, using the same instructions as above, this is how each second would be spent:
;
;Second   Worker 1   Worker 2   Done
;0        C          .
;1        C          .
;2        C          .
;3        A          F       C
;4        B          F       CA
;5        B          F       CA
;6        D          F       CAB
;7        D          F       CAB
;8        D          F       CAB
;9        D          .       CABF
;10        E          .       CABFD
;11        E          .       CABFD
;12        E          .       CABFD
;13        E          .       CABFD
;14        E          .       CABFD
;15        .          .       CABFDE
;
;Each row represents one second of time. The Second column identifies how many seconds
;have passed as of the beginning of that second. Each worker column shows the step that
;worker is currently doing (or . if they are idle). The Done column shows completed steps.
;
;Note that the order of the steps has changed; this is because steps now take time to finish
;and multiple workers can begin multiple steps simultaneously.
;
;In this example, it would take 15 seconds for two workers to complete these steps.
;
;With 5 workers and the 60+ second step durations described above, how long will it take
;to complete all of the steps?

(defn step-time
  [s t]
  (if s
    (+ t
       (- (int (first (name s)))
          (dec (int \A))))
    0))

(defn idle?
  [w]
  (nil? (last (second w))))

(defn done?
  [w time-offset]
  (and (not (idle? w))
       (let [queue (second w)
             step (last queue)
             num-steps (count (filter #(= step %) queue))]
         (>= num-steps (step-time step time-offset)))))

(defn process-one-sec
  [{:keys [accumulator workers evaluation-set forward-links back-links time-offset]}]
  (let [done-workers (filter #(done? % time-offset) workers)
        completed-steps (map #(last (second %)) done-workers)
        updated-accumulator (apply conj accumulator completed-steps)
        updated-eval-set (apply disj evaluation-set completed-steps)
        next-steps (->> (filter #(let [requirements (set (% back-links))]
                                   (or (nil? requirements)
                                       (= requirements
                                          (set/intersection requirements (set updated-accumulator))))) updated-eval-set)
                        sort)]
    (println "next-steps: " next-steps)
    (if (seq next-steps)
      (let [available-workers (into (sorted-map) (concat done-workers (filter idle? workers)))
            ; Update busy workers with the step that they're already working on.
            busy-workers (->> (apply dissoc workers (keys available-workers))
                              (map #(vector (first %) (conj (second %) (last (second %)))))
                              (into (sorted-map)))
            new-steps (take (count available-workers) next-steps)
            ; Update assigned workers with new steps.
            assigned-workers (->> (into (sorted-map) (take (count new-steps) available-workers))
                                  (map vector new-steps)
                                  (map #(vector (first (second %)) (conj (second (second %)) (first %))))
                                  (into {}))
            ; Update unused workers with nil at the end of the queue.
            unused-workers (->> (into {} (take-last (count new-steps) available-workers))
                                (map #(vector (first %) (conj (second %) nil)))
                                (into {}))]
        {:accumulator    updated-accumulator
         :workers        (conj busy-workers assigned-workers unused-workers)
         :evaluation-set (set/union updated-eval-set
                                    (apply sorted-set (apply concat (map forward-links new-steps))))
         :forward-links  forward-links
         :back-links     back-links
         :time-offset    time-offset})
      {:accumulator    updated-accumulator
       :workers        workers
       :evaluation-set updated-eval-set
       :forward-links  forward-links
       :back-links     back-links
       :time-offset    time-offset}))
  )

(defn part2
  [data num-workers time-offset]
  (let [back-links (links data second first)
        forward-links (links data first second)
        start-set (apply sorted-set (set/difference (set (keys forward-links)) (set (keys back-links))))]
    (take-while #(seq (:evaluation-set %)) (iterate process-one-sec {:accumulator    []
                                                                     :workers        (->> (take num-workers (repeat []))
                                                                                          (map vector (range 1 (inc num-workers)))
                                                                                          (into {}))
                                                                     :evaluation-set start-set
                                                                     :forward-links  forward-links
                                                                     :back-links     back-links
                                                                     :time-offset    time-offset}))))

