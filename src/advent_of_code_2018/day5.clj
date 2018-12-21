(ns advent-of-code-2018.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;--- Day 5: Alchemical Reduction ---
;You've managed to sneak in to the prototype suit manufacturing lab.
;The Elves are making decent progress, but are still struggling with the suit's size reduction capabilities.
;
;While the very latest in 1518 alchemical technology might have solved their problem eventually, you can do better.
;You scan the chemical composition of the suit's material and discover that it is formed by extremely long polymers (one of which is available as your puzzle input).
;
;The polymer is formed by smaller units which, when triggered, react with each other such that two adjacent units
;of the same type and opposite polarity are destroyed. Units' types are represented by letters; units' polarity is represented by capitalization.
;For instance, r and R are units with the same type but opposite polarity, whereas r and s are entirely different types and do not react.
;
;For example:
;
;In aA, a and A react, leaving nothing behind.
;In abBA, bB destroys itself, leaving aA. As above, this then destroys itself, leaving nothing.
;In abAB, no two adjacent units are of the same type, and so nothing happens.
;In aabAAB, even though aa and AA are of the same type, their polarities match, and so nothing happens.
;Now, consider a larger example, dabAcCaCBAcCcaDA:
;
;dabAcCaCBAcCcaDA  The first 'cC' is removed.
;dabAaCBAcCcaDA    This creates 'Aa', which is removed.
;dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
;dabCBAcaDA        No further actions can be taken.
;After all possible reactions, the resulting polymer contains 10 units.
;
;How many units remain after fully reacting the polymer you scanned?

(defn react-pair
  [[c1 c2]]
  (let [pair [c1 c2]]
    (if (and (> (count (set pair)) 1)
             (= (count (set (map clojure.string/lower-case pair))) 1))
      nil
      pair)))

;
; Reduce function:
;
; Compare last-prev and first-next chars.
; If they react:
;    - Pop last from prev and first from next.
; Pass updated prev/next to next reduce stage. ;
;
; Run reduce function until input = output.
;
(defn polymer-react
  [s]
  (reduce (fn [accum c]
            (if (seq accum)
              (let [p [(last accum) c]
                    r (react-pair p)]
                ;(println "accum: " accum)
                ;(println "c: " c)
                ;(println "p: " p)
                ;(println "r: " r)
                (if r
                  (conj accum c)
                  (vec (drop-last accum))))
              [c]))
          [(first s)]
          (rest s)))

(defn part1
  [path]
  (->> (io/resource path)
       io/reader
       slurp
       str/trim
       polymer-react
       count))