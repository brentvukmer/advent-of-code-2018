(ns advent-of-code-2018.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util GregorianCalendar Calendar)))

;--- Day 4: Repose Record ---
;You've sneaked into another supply closet - this time, it's across from the prototype suit manufacturing lab.
;You need to sneak inside and fix the issues with the suit, but there's a guard stationed outside the lab,
;so this is as close as you can safely get.
;
;As you search the closet for anything that might help, you discover that you're not the first person to want
;to sneak in. Covering the walls, someone has spent an hour starting every midnight for the past few months
;secretly observing this guard post! They've been writing down the ID of the one guard on duty that night -
;the Elves seem to have decided that one guard was enough for the overnight shift - as well as when they
;fall asleep or wake up while at their post (your puzzle input).
;
;For example, consider the following records, which have already been organized into chronological order:
;
;[1518-11-01 00:00] Guard #10 begins shift
;[1518-11-01 00:05] falls asleep
;[1518-11-01 00:25] wakes up
;[1518-11-01 00:30] falls asleep
;[1518-11-01 00:55] wakes up
;[1518-11-01 23:58] Guard #99 begins shift
;[1518-11-02 00:40] falls asleep
;[1518-11-02 00:50] wakes up
;[1518-11-03 00:05] Guard #10 begins shift
;[1518-11-03 00:24] falls asleep
;[1518-11-03 00:29] wakes up
;[1518-11-04 00:02] Guard #99 begins shift
;[1518-11-04 00:36] falls asleep
;[1518-11-04 00:46] wakes up
;[1518-11-05 00:03] Guard #99 begins shift
;[1518-11-05 00:45] falls asleep
;[1518-11-05 00:55] wakes up

;Timestamps are written using year-month-day hour:minute format.
;The guard falling asleep or waking up is always the one whose shift most recently started.
;Because all asleep/awake times are during the midnight hour (00:00 - 00:59),
;only the minute portion (00 - 59) is relevant for those events.
;
;Visually, these records show that the guards are asleep at these times:
;
;Date   ID   Minute
;000000000011111111112222222222333333333344444444445555555555
;012345678901234567890123456789012345678901234567890123456789
;11-01  #10  .....####################.....#########################.....
;11-02  #99  ........................................##########..........
;11-03  #10  ........................#####...............................
;11-04  #99  ....................................##########..............
;11-05  #99  .............................................##########.....

;The columns are Date, which shows the month-day portion of the relevant day;
;ID, which shows the guard on duty that day; and Minute, which shows the minutes
;during which the guard was asleep within the midnight hour.
;(The Minute column's header shows the minute's ten's digit in the first row and the one's digit in the second row.)
;Awake is shown as ., and asleep is shown as #.
;
;Note that guards count as asleep on the minute they fall asleep, and they count as awake on the minute they wake up.
; For example, because Guard #10 wakes up at 00:25 on 1518-11-01, minute 25 is marked as awake.
;
;If you can figure out the guard most likely to be asleep at a specific time, you might be able to trick that guard
; into working tonight so you can have the best chance of sneaking in. You have two strategies for choosing the best guard/minute combination.
;

; PART 1
;
;Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?
;
;In the example above, Guard #10 spent the most minutes asleep, a total of 50 minutes (20+25+5), while Guard #99 only slept
; for a total of 30 minutes (10+10+10). Guard #10 was asleep most during minute 24 (on two days, whereas any other minute the
; guard was asleep was only seen on one day).
;
;While this example listed the entries in chronological order, your entries are in the order you found them.
; You'll need to organize them before they can be analyzed.
;
;What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 10 * 24 = 240.)

(defn parse-julian-date-time
  [s]
  (let [[f & r] (first (re-seq #"\[(\d+)\-(\d+)\-(\d+) (\d+)\:(\d+)\]" s))
        jdt (->> r
                 (map #(Integer/valueOf %))
                 (apply (fn [year month day hour minute] (new GregorianCalendar year month day hour minute))))]
    {:date-time jdt
     :remaining (str/replace s f "")}))

(defn parse-guard-id
  [s]
  (let [match (->> (re-seq #"(?:\#\d+)?" s)
                   (remove empty?)
                   first)]
    (if match
      (-> (str/replace match "#" "")
          read-string)
      match)))

(defn parse-wake-state
  [s]
  (let [match (->> (re-seq #"(?:\w+\s\w+)?" s)
                   (remove empty?)
                   first)]
    (-> match
        (str/replace " " "-")
        keyword)))

(defn parse-input-row
  [r]
  (let [{:keys [date-time remaining]} (parse-julian-date-time r)
        guard-id (parse-guard-id remaining)]
    (if guard-id
      {:date-time date-time
       :guard-id  guard-id}
      {:date-time date-time
       :action    (parse-wake-state remaining)})))

(defn read-raw-data
  [path]
  (with-open [rdr (io/reader (io/resource path))]
    (->> (map parse-input-row (line-seq rdr))
         (sort-by :date-time))))

;
; Parse each input data row into a map.
; Sort maps by :date-time.
; Run reduce on the maps, building up a map of guard id
; to a sorted map of days to sleep intervals.
; Find the guard who slept the most minutes.
; Build up a map of minute to count (i.e. "guard X was asleep 3 times at minute 12.")
; Return guard id multiplied by the minute with the max count.
;
(defn guard-sleep-intervals
  [data]
  (->
    (reduce
      (fn [accum x] (let [guard-id (:guard-id x)
                          current-guard-id (:current-guard-id accum)]
                      (if guard-id
                        (assoc accum :current-guard-id guard-id
                                     guard-id (get accum guard-id []))
                        (update accum
                                current-guard-id
                                (fn [y] (conj (get accum current-guard-id) [(:date-time x) (:action x)]))))))
      {}
      data)
    (dissoc :current-guard-id)))

(defn guard-sleep-minutes
  [data]
  (->> (for [[id intervals] (guard-sleep-intervals data)
             :let [date-times
                   (->> intervals
                        flatten
                        (remove keyword?))
                   minute-ranges (->> (map #(.get % (Calendar/MINUTE)) date-times)
                                      (partition 2)
                                      (map #(apply range %)))]]
         [id minute-ranges])
       (into {})))

(defn guard-sleep-max
  [data]
  (->> (for [[id minute-ranges] (guard-sleep-minutes data)
             :let [sum (->> (map #(- (last %) (first %)) minute-ranges)
                            (apply +))
                   freqs (->> (apply concat minute-ranges)
                              (group-by identity)
                              (sort-by #(count (second %))))]]
         {:id    id
          :sum   sum
          :freqs freqs})
       (sort-by :sum)
       last))

(defn part1
  [data]
  (let [max (guard-sleep-max data)]
    (* (:id max) (first (last (:freqs max))))))