(ns lambda-chess.core-test
  (:require [clojure.test :refer :all]
            [lambda-chess.core :refer :all])
  (:import (lambda_chess.core Piece)))

(deftest square-names-test
  (testing "contains 64 square names"
    (is (= 64 (count square-names)))))

(deftest empty-board-test
  (testing "contains 64 squares"
    (is (= 64 (count empty-board)))))

(deftest white-pawn-moves-test
  (testing "pawn moves from a2"
    (is (= [:a3 :a4] (white-pawn-moves :a2))))
  (testing "pawn moves from b4"
    (is (= [:b5] (white-pawn-moves :b4)))))

(deftest black-pawn-moves-test
  (testing "black pawn moves from a7"
    (is (= [:a6 :a5] (black-pawn-moves :a7))))
  (testing "black pawn moves from a6"
    (is (= [:a5] (black-pawn-moves :a6)))))

(deftest rook-moves-test
  (testing "rook moves from a1"
    (is (= [:a2 :a3 :a4 :a5 :a6 :a7 :a8 :b1 :c1 :d1 :e1 :f1 :g1 :h1] (rook-moves :a1))))
  (testing "rook moves from d4"
    (is (= [:d1 :d2 :d3 :d5 :d6 :d7 :d8 :a4 :b4 :c4 :e4 :f4 :g4 :h4] (rook-moves :d4)))))

(deftest abs-test
  (testing "our abs func"
    (is (= 1 (abs -1))))
  (testing "our abs func"
    (is (= 1 (abs 1)))))

(deftest bishop-moves-test
  (testing "testing bishop moves from a5"
    (is (= [:b4 :b6 :c3 :c7 :d2 :d8 :e1] (bishop-moves :a5))))
  (testing "testing bishop moves from d4"
    (is (= [:a1 :a7 :b2 :b6 :c3 :c5 :e3 :e5 :f2 :f6 :g1 :g7 :h8] (bishop-moves :d4)))))

(deftest queen-moves-test
  (testing "testing queen moves from a1"
    (is (= [:b2 :c3 :d4 :e5 :f6 :g7 :h8 :a2 :a3 :a4 :a5 :a6 :a7 :a8 :b1 :c1 :d1 :e1 :f1 :g1 :h1]
           (queen-moves :a1))))
  (testing "testing queen moves from f3"
    (is (= [:a8 :b7 :c6 :d1 :d5 :e2 :e4 :g2 :g4 :h1 :h5 :f1 :f2 :f4 :f5 :f6 :f7 :f8 :a3 :b3 :c3 :d3 :e3 :g3 :h3]
           (queen-moves :f3)))))

(deftest king-moves-test
  (testing "testing king moves from a3"
    (is (= [:a2 :a4 :b2 :b3 :b4] (king-moves :a3))))
  (testing "testing king moves from d4"
    (is (= [:c3 :c4 :c5 :d3 :d5 :e3 :e4 :e5] (king-moves :d4)))))

(deftest knight-moves-test
  (testing "testing knight moves from a1"
    (is (= [:b3 :c2] (knight-moves :a1))))
  (testing "testing knight moves from d4"
    (is (= [:b3 :b5 :c2 :c6 :e2 :e6 :f3 :f5] (knight-moves :d4)))))

(deftest white-pawn-possible-moves-test
  (testing "testing white pawn moves from a2"
    (is (= [:a3 :a4] (white-pawn-possible-moves :a2 empty-board))))
  (testing "testing white pawn moves from a2, but there is our pawn on a4"
    (is (= [:a3] (white-pawn-possible-moves :a2 (place-piece empty-board :a4 (Piece. pawn white "♙"))))))
  (testing "testing white pawn moves from a2, but we can take black pawn on b3"
    (is (= [:a3 :a4 :b3] (white-pawn-possible-moves :a2 (place-piece empty-board :b3 (Piece. pawn black "♟")))))))

(deftest black-pawn-captures-test
  (testing "testing black pawn captures from a7"
    (is (= [:b6] (black-pawn-captures :a7 (place-piece empty-board :b6 (Piece. pawn white "♙")))))))

(deftest black-pawn-possible-moves-test
  (testing "testing black pawn moves from a7"
    (is (= [:a6 :a5] (black-pawn-possible-moves :a7 empty-board))))
  (testing "testing black pawn moves from a7, but there is our pawn on a5"
    (if [:a6] (black-pawn-possible-moves :a7 (place-piece empty-board :a5 "♟")))))

(deftest rook-directions-test
  (testing "testing rook directions from d4"
    (is (= {:top [:d5 :d6 :d7 :d8] :downward [:d1 :d2 :d3] :left [:a4 :b4 :c4] :right [:e4 :f4 :g4 :h4]} (rook-directions :d4)))))

(deftest white-rook-possible-moves-test
  (testing "testing white rook possible moves from d4"
    (is  (= [:d5 :d6 :d7 :d8 :d1 :d2 :d3 :a4 :b4 :c4 :e4 :f4 :g4 :h4] (white-rook-possible-moves :d4 empty-board))))
  (testing "testing white rook possible moves from d4 but there is white pawn on d6"
    (is (= [:d5 :d1 :d2 :d3 :a4 :b4 :c4 :e4 :f4 :g4 :h4]
           (white-rook-possible-moves :d4 (place-piece empty-board :d6 (Piece. pawn white "♙")))))))

(deftest black-rook-possible-moves-test
  (testing "testing black rook possible moves from d4"
    (is  (= [:d5 :d6 :d7 :d8 :d1 :d2 :d3 :a4 :b4 :c4 :e4 :f4 :g4 :h4] (black-rook-possible-moves :d4 empty-board))))
  (testing "testing black rook possible moves from d4 but there is white pawn on d6"
    (is (= [:d6 :d5 :d1 :d2 :d3 :a4 :b4 :c4 :e4 :f4 :g4 :h4]
           (black-rook-possible-moves :d4 (place-piece empty-board :d6 (Piece. pawn white "♙")))))))

(deftest bishop-directions-test
  (testing "testing bishop directions"
    (is (= {:top-right [:e5 :f6 :g7 :h8] :top-left [:a7 :b6 :c5] :down-right [:e3 :f2 :g1] :down-left [:a1 :b2 :c3]} (bishop-directions :d4)))))

(deftest bishop-possible-moves-test
  (testing "white bishop possible moves from d4, but there is a black pawn on g7"
    (is (= [:e5 :f6 :g7 :a7 :b6 :c5 :e3 :f2 :g1 :a1 :b2 :c3]
           (bishop-possible-moves :d4 (place-piece empty-board :g7 (Piece. pawn black "♟")) white)))))
