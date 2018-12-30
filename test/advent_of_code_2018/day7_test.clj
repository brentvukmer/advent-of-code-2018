(ns advent-of-code-2018.day7-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2018.day7 :refer :all]))

(deftest step-zero
  (testing "The output for step zero is the correct input for step one."
    (is (= {:accumulator [],
            :workers {1 [:C], 2 [nil]},
            :evaluation-set #{:A :C :F},
            :forward-links {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
            :back-links {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
            :time-offset 0}
           (process-one-sec {:accumulator [],
                             :workers {1 [], 2 []},
                             :evaluation-set #{:C},
                             :forward-links {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
                             :back-links {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
                             :time-offset 0})))))

(deftest step-one
  (testing "The output for step one is the correct input for step two."
    (is (= {:accumulator [],
            :workers {1 [:C :C], 2 [nil nil]},
            :evaluation-set #{:A :B :C :D :E :F},
            :forward-links {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
            :back-links {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
            :time-offset 0}
           (process-one-sec {:accumulator [],
                             :workers {1 [:C], 2 [nil]},
                             :evaluation-set #{:A :C :F},
                             :forward-links {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
                             :back-links {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
                             :time-offset 0})))))

(deftest step-two
  (testing "The output for step two is the correct input for step three."
    (is (= {:accumulator [],
            :workers {1 [:C :C :C], 2 [nil nil nil]},
            :evaluation-set #{:A :B :C :D :E :F},
            :forward-links {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
            :back-links {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
            :time-offset 0}
           (process-one-sec {:accumulator [],
                             :workers {1 [:C :C], 2 [nil nil]},
                             :evaluation-set #{:A :C :F},
                             :forward-links {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
                             :back-links {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
                             :time-offset 0})))))

(deftest step-three
  (testing "The output for step three is the correct input for step four."
    (is (= {:accumulator [:C],
            :workers {1 [:C :C :C :A], 2 [nil nil nil nil]},
            :evaluation-set #{:A :B :D :E :F},
            :forward-links {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
            :back-links {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
            :time-offset 0}
           (process-one-sec {:accumulator [],
                             :workers {1 [:C :C :C], 2 [nil nil nil]},
                             :evaluation-set #{:A :C :F},
                             :forward-links {:C '(:A :F), :A '(:B :D), :B '(:E), :D '(:E), :F '(:E)},
                             :back-links {:A '(:C), :F '(:C), :B '(:A), :D '(:A), :E '(:B :D :F)},
                             :time-offset 0})))))