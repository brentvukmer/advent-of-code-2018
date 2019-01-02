(ns advent-of-code-2018.day9
  (:require [clojure.set :as set]
            [clojure.java.io :as io]))

;PART 1
;
;You talk to the Elves while you wait for your navigation system to initialize.
;To pass the time, they introduce you to their favorite marble game.
;
;The Elves play this game by taking turns arranging the marbles in a circle
;according to very particular rules. The marbles are numbered starting with 0
;and increasing by 1 until every marble has a number.
;
;First, the marble numbered 0 is placed in the circle. At this point, while
;it contains only a single marble, it is still a circle: the marble is both
;clockwise from itself and counter-clockwise from itself.
;This marble is designated the current marble.
;
;Then, each Elf takes a turn placing the lowest-numbered remaining marble into
;the circle between the marbles that are 1 and 2 marbles clockwise of the current
;marble. (When the circle is large enough, this means that there is one marble
;between the marble that was just placed and the current marble.) The marble that
;was just placed then becomes the current marble.
;
;However, if the marble that is about to be placed has a number which is a
;multiple of 23, something entirely different happens. First, the current player
;keeps the marble they would have placed, adding it to their score.
;In addition, the marble 7 marbles counter-clockwise from the current marble is
;removed from the circle and also added to the current player's score. The marble
;located immediately clockwise of the marble that was removed becomes the new
;current marble.
;
;For example, suppose there are 9 players. After the marble with value 0 is
;placed in the middle, each player (shown in square brackets) takes a turn.
;The result of each of those turns would produce circles of marbles like this,
;where clockwise is to the right and the resulting current marble is in
;parentheses:
;
;[-] (0)
;[1]  0 (1)
;[2]  0 (2) 1
;[3]  0  2  1 (3)
;[4]  0 (4) 2  1  3
;[5]  0  4  2 (5) 1  3
;[6]  0  4  2  5  1 (6) 3
;[7]  0  4  2  5  1  6  3 (7)
;[8]  0 (8) 4  2  5  1  6  3  7
;[9]  0  8  4 (9) 2  5  1  6  3  7
;[1]  0  8  4  9  2(10) 5  1  6  3  7
;[2]  0  8  4  9  2 10  5(11) 1  6  3  7
;[3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
;[4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
;[5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
;[6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
;[7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
;[8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
;[9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
;[1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
;[2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
;[3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
;[4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
;[5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
;[6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
;[7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15
;
;The goal is to be the player with the highest score after the last marble is
;used up. Assuming the example above ends after the marble numbered 25, the
;winning score is 23+9=32 (because player 5 kept marble 23 and removed marble 9,
;while no other player got any points in this very short example game).
;
;Here are a few more examples:
;
;10 players; last marble is worth 1618 points: high score is 8317
;13 players; last marble is worth 7999 points: high score is 146373
;17 players; last marble is worth 1104 points: high score is 2764
;21 players; last marble is worth 6111 points: high score is 54718
;30 players; last marble is worth 5807 points: high score is 37305
;
;What is the winning Elf's score?

(defn read-data
  [path]
  (let [rows
        (with-open [rdr (io/reader (io/resource path))]
          (->> (map #(->> (re-seq #"(\d+) players; last marble is worth (\d+) points" %)
                          first
                          rest
                          (mapv read-string))
                    (line-seq rdr))
               vec))]
    (mapv #(hash-map :num-players (first %)
                     :num-marbles (inc (second %)))
          rows)))

(defn next-clockwise-index
  [v i]
  (mod (inc i) (count v)))

(defn counter-clockwise-index
  [v i d]
  (mod (- i d) (count v)))

(defn index-of-m
  [v m]
  (->> (keep-indexed #(if (= m %2) %1) v)
       first))

(defn insert-m
  [v m i]
  (->> (concat (take (inc i) v) (cons m (drop (inc i) v)))
       vec))

(defn remove-ccw-m
  [v m]
  (let [current-index (index-of-m v m)]
    (if current-index
      (let [i (counter-clockwise-index v current-index 7)]
        (->> (concat (take i v) (drop (inc i) v))
             vec))
      v)))

(defn play-circle
  [{:keys [circle current-marble marbles player-marbles player num-players num-marbles]}]
  (let [current-player (if player
                         (mod (inc player) num-players)
                         1)
        next-marble (first marbles)
        take-marbles? (and (pos-int? next-marble)
                           (= 0 (mod next-marble 23)))
        updates (if take-marbles?
                  (let [updated-circle (remove-ccw-m circle current-marble)
                        removed-marble (->> (set/difference (set circle) (set updated-circle))
                                            first)]
                    {:circle         updated-circle
                     :player-marbles (->> removed-marble
                                          (conj (get player-marbles current-player []) next-marble)
                                          (assoc player-marbles current-player))
                     :current-marble (->> (next-clockwise-index circle (index-of-m circle removed-marble))
                                          (get circle))})
                  {:circle         (->> (index-of-m circle current-marble)
                                        (next-clockwise-index circle)
                                        (insert-m circle next-marble))
                   :player-marbles player-marbles
                   :current-marble next-marble})]
    {:circle         (:circle updates)
     :current-marble (:current-marble updates)
     :marbles        (rest marbles)
     :player-marbles (:player-marbles updates)
     :player         current-player
     :num-players    num-players
     :num-marbles    num-marbles}))

(defn game-scores
  [{:keys [player-marbles]}]
  (->> (vals player-marbles)
       (map #(apply + %))
       sort))

(defn initial-state
  [{:keys [num-players num-marbles]}]
  {:circle         [0],
   :current-marble 0,
   :marbles        (range 1 num-marbles),
   :player-marbles {},
   :player         nil,
   :num-players    num-players
   :num-marbles    num-marbles})

(defn data-at
  [path r]
  (-> (read-data path)
       (get r)
       initial-state))

(defn part1
  ([path r]
   (let [state-zero (data-at path r)]
     (->> state-zero
          (iterate play-circle)
          (take (:num-marbles state-zero))
          last
          game-scores
          last)))
  ([path]
    (part1 path 0)))