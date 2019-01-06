(ns advent-of-code-2018.day9-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2018.day9 :refer :all]))

(deftest test-9-25
  (testing "Testing 9 players 25 max marbles value."
    (is (= 32 (part2 9 25)))))

(deftest test-10-1618
  (testing "Testing 10 players 1618 max marbles value."
    (is (= 8317 (part2 10 1618)))))

(deftest test-13-7999
  (testing "Testing 13 players 7999 max marbles value."
    (is (= 146373 (part2 13 7999)))))

(deftest test-17-1104
  (testing "Testing 17 players 1104 max marbles value."
    (is (= 2764 (part2 17 1104)))))

(deftest test-21-6111
  (testing "Testing 13 players 7999 max marbles value."
    (is (= 54718 (part2 21 6111)))))

(deftest test-30-5807
  (testing "Testing 13 players 7999 max marbles value."
    (is (= 37305 (part2 30 5807)))))

; Verify that we get the same answer for part1 prod inputs
(deftest test-476-71657
  (testing "Testing 13 players 7999 max marbles value."
    (is (= 386018 (part2 476 71657)))))

(deftest test-476-100-times-71657
  (testing "Testing 13 players 7999 max marbles value."
    (is (= 3085518618 (part2 476 (* 100 71657))))))

