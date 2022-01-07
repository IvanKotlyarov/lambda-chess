(ns lambda-chess.core-test
  (:require [clojure.test :refer :all]
            [lambda-chess.core :refer :all])
  (:import (lambda_chess.core Piece Move)))

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
    (is (= [:b2 :c3 :d4 :e5 :f6 :g7 :h8 :a2 :a3 :a4 :a5 :a6 :a7 :a8 :b1 :c1 :d1 :e1 :f1 :g1 :h1] (queen-moves :a1))))
  (testing "testing queen moves from f3"
    (is (= [:a8 :b7 :c6 :d1 :d5 :e2 :e4 :g2 :g4 :h1 :h5 :f1 :f2 :f4 :f5 :f6 :f7 :f8 :a3 :b3 :c3 :d3 :e3 :g3 :h3] (queen-moves :f3)))))

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

(deftest pawn-captures-test
  (testing "pawn captures"
    (is (= [:d5]
           (pawn-captures :e4 (place-piece
                                  (place-piece empty-board :e4 (Piece. pawn white "P")) :d5 (Piece. pawn black "p")) white)))))

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
    (is (= {:top [:d5 :d6 :d7 :d8] :downward [:d1 :d2 :d3] :left [:a4 :b4 :c4] :right [:e4 :f4 :g4 :h4]}
           (rook-directions :d4)))))

(deftest white-rook-possible-moves-test
  (testing "testing white rook possible moves from d4"
    (is  (= [:d5 :d6 :d7 :d8 :d3 :d2 :d1 :c4 :b4 :a4 :e4 :f4 :g4 :h4] (white-rook-possible-moves :d4 empty-board))))
  (testing "testing white rook possible moves from d4 but there is white pawn on d6"
    (is (= [:d5 :d3 :d2 :d1 :c4 :b4 :a4 :e4 :f4 :g4 :h4]
           (white-rook-possible-moves :d4 (place-piece empty-board :d6 (Piece. pawn white "♙"))))))
  (testing "testing white rook possible moves from d4, but there is white pawn on b4"
    (is (= [:d5 :d6 :d7 :d8 :d3 :d2 :d1 :c4 :e4 :f4 :g4 :h4]
           (white-rook-possible-moves :d4 (place-piece empty-board :b4 (Piece. pawn white 'p'))))))
  (testing "testing white rook possible moves from d4, but there is black pawn on b4"
    (is (= [:d5 :d6 :d7 :d8 :d3 :d2 :d1 :c4 :b4 :e4 :f4 :g4 :h4]
           (white-rook-possible-moves :d4 (place-piece empty-board :b4 (Piece. pawn black 'p')))))))

(deftest black-rook-possible-moves-test
  (testing "testing black rook possible moves from d4"
    (is  (= [:d5 :d6 :d7 :d8 :d3 :d2 :d1 :c4 :b4 :a4 :e4 :f4 :g4 :h4] (black-rook-possible-moves :d4 empty-board))))
  (testing "testing black rook possible moves from d4 but there is white pawn on d6"
    (is (= [:d5 :d6 :d3 :d2 :d1 :c4 :b4 :a4 :e4 :f4 :g4 :h4]
           (black-rook-possible-moves :d4 (place-piece empty-board :d6 (Piece. pawn white "♙")))))))

(deftest bishop-directions-test
  (testing "testing bishop directions"
    (is (=
          {:top-right [:e5 :f6 :g7 :h8] :top-left [:a7 :b6 :c5] :down-right [:e3 :f2 :g1] :down-left [:a1 :b2 :c3]} (bishop-directions :d4)))))

(deftest bishop-possible-moves-test
  (testing "white bishop possible moves from d4, but there is a black pawn on g7"
    (is (= [:e5 :f6 :g7 :a7 :b6 :c5 :e3 :f2 :g1 :a1 :b2 :c3]
           (bishop-possible-moves :d4 (place-piece empty-board :g7 (Piece. pawn black "♟")) white))))
  (testing "white bishop possible moves from a1, but there is our pawn on d4"
    (is (= [:b2 :c3] (bishop-possible-moves :a1 (place-piece empty-board :d4 (Piece. pawn white "P")) white)))))

(deftest knight-possible-moves-test
  (testing "white knight moves from d4, but there is a white pawn on c2"
    (is (= [:b3 :b5 :c6 :e2 :e6 :f3 :f5] (knight-possible-moves :d4 (place-piece empty-board :c2 (Piece. pawn white "P")) white))))
  (testing "knight possible-moves from a1"
    (is (= [:b3 :c2] (knight-possible-moves :a1 empty-board white)))))

(deftest queen-possible-moves-test
  (testing "white queen possible moves from a1"
    (is (= [:a2 :a3 :a4 :a5 :a6 :a7 :a8 :b1 :c1 :d1 :e1 :f1 :g1 :h1 :b2 :c3 :d4 :e5 :f6 :g7 :h8]
           (queen-possible-moves :a1 empty-board white))))
  (testing "white queen possible moves, but there is our pawn on d4"
    (is (= [:a2 :a3 :a4 :a5 :a6 :a7 :a8 :b1 :c1 :d1 :e1 :f1 :g1 :h1 :b2 :c3]
           (queen-possible-moves :a1 (place-piece empty-board :d4 (Piece. pawn white "p")) white)))))

(deftest possible-moves-squares-test
  (testing "possible moves' squares"
    (is (= [:b2 :c3 :d4 :e5 :f6 :g7 :h8] (possible-moves-squares :a1 (place-piece empty-board :a1 (Piece. bishop white "b")) white))))
  (testing "weird case: get null pointer exception for king at :a1"
    (is (= [:a2 :b1 :b2]
           (possible-moves-squares :a1 (place-piece
                                         (place-piece empty-board :a1 (Piece. king white "K")) :b2 (Piece. pawn black "p")) white)))))

(deftest possible-moves-test
  (testing "possible moves test"
    (is (= [
            (Move. (Piece. king white "K") :a1 :a2 nil)
            (Move. (Piece. king white "K") :a1 :b1 nil )
            (Move. (Piece. king white "K") :a1 :b2 nil)]
           (possible-moves :a1 (place-piece empty-board :a1 (Piece. king white "K")) white)))))

(deftest pieces-captures-test
  (testing "pawn captures"
    (is (= #{:d5}
           (pieces-captures (place-piece
                              (place-piece empty-board :e4 (Piece. pawn white "P")) :d5 (Piece. pawn black "p")) white false))))
  (testing "knight captures"
    (is (= #{:c2 :b3}
           (pieces-captures
             (place-piece (place-piece empty-board :a1 (Piece. knight white "N")) :c2 (Piece. pawn black "p")) white false)))))

(deftest king-possible-moves-test
  (testing "testing king possible moves, but there is our rook on a2"
    (is (= [:b1 (king-possible-moves :a1 (place-piece empty-board :a2 (Piece. rook white "R")) white)])))
  (testing "inf loop"
    (is (= [:e2 (king-possible-moves :e1
                                     (place-piece (place-piece initial-board :e4 (Piece. pawn white "P")) :e2 nil) white)]))))

(deftest white-castling-test
  (testing "testing can white castling"
    (is (= (assoc (assoc empty-board :g1 (Piece. king white "K")) :f1 (Piece. rook white "R"))
           (white-castling (Move. (Piece. king white "K") :e1 :g1 nil)
                           (assoc (assoc empty-board :e1 (Piece. king white "K")) :h1 (Piece. rook white "R"))
                           start-game-state)))))

(deftest black-castling-test
  (testing "testing can black castling"
    (is (= (assoc (assoc empty-board :g8 (Piece. king black "k")) :f8 (Piece. rook black "r"))
           (black-castling (Move. (Piece. king black "k") :e8 :g8 nil)
                           (assoc (assoc empty-board :e8 (Piece. king black "k")) :h8 (Piece. rook black "r"))
                           start-game-state)))))

(deftest pawn-promotion-test
  (testing "testing pawn promotion"
    (is (= (:a8 (pawn-promotion :a8 empty-board (Piece. rook white "R"))) (Piece. rook white "R")))))

(deftest initial-board-test
  (testing "squares count"
    (is (= 64 (count initial-board))))
  (testing "what is on a1"
    (is (= (Piece. rook white "R") (:a1 initial-board)))))

(deftest check?-test
  (testing "check by queen"
    (is (check? (place-piece (place-piece empty-board :a1 (Piece. king white "K")) :a8 (Piece. queen black "q")) white)))
  (testing "check by black knight"
    (is (check? (place-piece (place-piece empty-board :a1 (Piece. king white "K")) :c2 (Piece. knight black "n")) white)))
  (testing "check by white knight"
    (is (check? (place-piece (place-piece empty-board :a1 (Piece. king black "K")) :c2 (Piece. knight white "n")) black)))
  (testing "check by rook"
    (is (check? (place-piece (place-piece empty-board :a1 (Piece. king white "K")) :a8 (Piece. rook black "r")) white)))
  (testing "check by bishop"
    (is (check? (place-piece (place-piece empty-board :a1 (Piece. king white "K")) :h8 (Piece. bishop black "b")) white)))
  (testing "check by pawn"
    (is (check? (place-piece (place-piece empty-board :a1 (Piece. king white "K")) :b2 (Piece. pawn black "p")) white)))
  (testing "inf loop on initial position"
    (is (not (check? (place-piece (place-piece initial-board :e4 (Piece. pawn white "P")) :e2 nil) white)))))

(deftest checkmate-test
  (testing "checkmate by two black rooks"
    (is (= [] (checkmate
                (place-piece
                  (place-piece
                    (place-piece empty-board :a1 (Piece. king white "K")) :a8 (Piece. rook black "r")) :b8 (Piece. rook black "r")) white))))
  (testing "just check"
    (is (= [(Move. (Piece. rook white "R") :h2 :a2 nil)] (checkmate (place-piece
                           (place-piece
                             (place-piece
                               (place-piece empty-board :a1
                                            (Piece. king white "K")) :a8
                               (Piece. rook black "r")) :b8
                             (Piece. rook black "r"))
                           :h2 (Piece. rook white "R")) white)))))

(deftest valid-move?-test
  (testing "move is valid"
    (is (valid-move? (Move. (Piece. pawn white "P") :e2 :e4 nil) initial-board white))))

(deftest make-move-test
  (testing "pawn e2-e4"
    (is (= [(place-piece (place-piece initial-board :e4 (Piece. pawn white "P")) :e2 nil) start-game-state
            [(Move. (Piece. pawn white "P") :e2 :e4 nil)] true]
           (make-move (Move. (Piece. pawn white "P") :e2 :e4 nil) initial-board white start-game-state start-moves-history))))
  (testing "castling"
    (is (= [(assoc (assoc empty-board :g1 (Piece. king white "K")) :f1 (Piece. rook white "R"))
            {:white-kingside-castling false :white-queenside-castling false :black-kingside-castling true :black-queenside-castling true}
            [(Move. (Piece. king white "K") :e1 :g1 nil)] true]
           (make-move (Move. (Piece. king white "K") :e1 :g1 nil)
                      (assoc (assoc empty-board :e1 (Piece. king white "K")) :h1 (Piece. rook white "R"))
                      white start-game-state start-moves-history))))
  (testing "promotion"
    (is (= [(place-piece empty-board :a8 (Piece. queen white "Q"))
            start-game-state
            [(Move. (Piece. pawn white "P") :a7 :a8 (Piece. queen white "Q"))]
            true]
           (make-move (Move. (Piece. pawn white "P") :a7 :a8 (Piece. queen white "Q"))
                      (place-piece empty-board :a7 (Piece. pawn white "P"))
                      white start-game-state start-moves-history))))
  (testing "en-passant"
    (is (= [(place-piece empty-board :e6 (Piece. pawn white "P"))
            start-game-state
            [(Move. (Piece. pawn black "p") :e7 :e5 nil) (Move. (Piece. pawn white "P") :f5 :e6 nil)] true]
           (make-move (Move. (Piece. pawn white "P") :f5 :e6 nil)
                      (place-piece (place-piece empty-board :f5 (Piece. pawn white "P")) :e5 (Piece. pawn black "p"))
                      white start-game-state
                      [(Move. (Piece. pawn black "p") :e7 :e5 nil)])))))
