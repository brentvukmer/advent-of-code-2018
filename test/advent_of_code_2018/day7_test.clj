(ns advent-of-code-2018.day7-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2018.day7 :refer :all]))

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

(def test-states [{:accumulator [],
              :workers          {1 [], 2 []},
              :evaluation-set   #{},
              :forward-links    {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links       {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset      0},
             {:accumulator    [],
              :workers        {1 [:C], 2 [nil]},
              :evaluation-set #{:C},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [],
              :workers        {1 [:C :C], 2 [nil nil]},
              :evaluation-set #{:C},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [],
              :workers        {1 [:C :C :C], 2 [nil nil nil]},
              :evaluation-set #{:C},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C],
              :workers        {1 [:C :C :C :A], 2 [nil nil nil :F]},
              :evaluation-set #{:A :F},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A],
              :workers        {1 [:C :C :C :A :B], 2 [nil nil nil :F :F]},
              :evaluation-set #{:B :D :F},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A],
              :workers        {1 [:C :C :C :A :B :B], 2 [nil nil nil :F :F :F]},
              :evaluation-set #{:B :D :F},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A :B],
              :workers        {1 [:C :C :C :A :B :B :D], 2 [nil nil nil :F :F :F :F]},
              :evaluation-set #{:D :E :F},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A :B],
              :workers        {1 [:C :C :C :A :B :B :D :D], 2 [nil nil nil :F :F :F :F :F]},
              :evaluation-set #{:D :E :F},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A :B],
              :workers        {1 [:C :C :C :A :B :B :D :D :D], 2 [nil nil nil :F :F :F :F :F :F :F]},
              :evaluation-set #{:D :E :F},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A :B :F],
              :workers        {1 [:C :C :C :A :B :B :D :D :D :D], 2 [nil nil nil :F :F :F :F :F :F nil]},
              :evaluation-set #{:D :E},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A :B :F :D],
              :workers        {1 [:C :C :C :A :B :B :D :D :D :D :E], 2 [nil nil nil :F :F :F :F :F :F nil nil]},
              :evaluation-set #{:E},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A :B :F :D],
              :workers        {1 [:C :C :C :A :B :B :D :D :D :D :E :E], 2 [nil nil nil :F :F :F :F :F :F nil nil nil]},
              :evaluation-set #{:E},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A :B :F :D],
              :workers        {1 [:C :C :C :A :B :B :D :D :D :D :E :E :E], 2 [nil nil nil :F :F :F :F :F :F nil nil nil nil]},
              :evaluation-set #{:E},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A :B :F :D],
              :workers        {1 [:C :C :C :A :B :B :D :D :D :D :E :E :E :E], 2 [nil nil nil :F :F :F :F :F :F nil nil nil nil nil]},
              :evaluation-set #{:E},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A :B :F :D],
              :workers        {1 [:C :C :C :A :B :B :D :D :D :D :E :E :E :E :E], 2 [nil nil nil :F :F :F :F :F :F nil nil nil nil nil nil]},
              :evaluation-set #{:E},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0},
             {:accumulator    [:C :A :B :F :D :E],
              :workers        {1 [:C :C :C :A :B :B :D :D :D :D :E :E :E :E :E nil], 2 [nil nil nil :F :F :F :F :F :F nil nil nil nil nil nil nil]},
              :evaluation-set #{},
              :forward-links  {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
              :back-links     {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
              :time-offset    0}])

(deftest step-zero
  (testing "The output for second zero is the correct input for second one."
    (is (= (process-one-sec (first test-states)) (second test-states)))))

(deftest step-one
  (testing "The output for second one is the correct input for second two."
    (is (= (process-one-sec (second test-states))
           (nth test-states 2)))))

(deftest step-two
  (testing "The output for second two is the correct input for second three."
    (is (= (process-one-sec (nth test-states 2))
           (nth test-states 3)))))

(deftest step-three
  (testing "The output for second three is the correct input for second four."
    (is (= (process-one-sec (nth test-states 3))
           (nth test-states 4)))))

(deftest step-four
  (testing "The output for second four is the correct input for second five."
    (is (= (process-one-sec (nth test-states 4))
           (nth test-states 5)))))

(deftest state-pairs
  (testing "The output"))