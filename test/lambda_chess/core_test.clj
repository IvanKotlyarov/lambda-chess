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
  (testing "bishop moves from a5"
    (is (= [:b4 :b6 :c3 :c7 :d2 :d8 :e1] (bishop-moves :a5))))
  (testing "bishop moves from d4"
    (is (= [:a1 :a7 :b2 :b6 :c3 :c5 :e3 :e5 :f2 :f6 :g1 :g7 :h8] (bishop-moves :d4)))))

(deftest queen-moves-test
  (testing "queen moves from a1"
    (is (= [:b2 :c3 :d4 :e5 :f6 :g7 :h8 :a2 :a3 :a4 :a5 :a6 :a7 :a8 :b1 :c1 :d1 :e1 :f1 :g1 :h1] (queen-moves :a1))))
  (testing "queen moves from f3"
    (is (= [:a8 :b7 :c6 :d1 :d5 :e2 :e4 :g2 :g4 :h1 :h5 :f1 :f2 :f4 :f5 :f6 :f7 :f8 :a3 :b3 :c3 :d3 :e3 :g3 :h3] (queen-moves :f3)))))

(deftest king-moves-test
  (testing "king moves from a3"
    (is (= [:a2 :a4 :b2 :b3 :b4] (king-moves :a3))))
  (testing "king moves from d4"
    (is (= [:c3 :c4 :c5 :d3 :d5 :e3 :e4 :e5] (king-moves :d4)))))

(deftest knight-moves-test
  (testing "knight moves from a1"
    (is (= [:b3 :c2] (knight-moves :a1))))
  (testing "knight moves from d4"
    (is (= [:b3 :b5 :c2 :c6 :e2 :e6 :f3 :f5] (knight-moves :d4)))))

(deftest pawn-captures-test
  (testing "pawn captures"
    (is (= [:d5]
           (pawn-captures :e4 (place-piece
                                  (place-piece empty-board :e4 white-pawn) :d5 black-pawn) white)))))

(deftest white-pawn-possible-moves-test
  (testing "white pawn moves from a2"
    (is (= [(Move. white-pawn :a2 :a3 nil) (Move. white-pawn :a2 :a4 nil)]
           (white-pawn-possible-moves :a2 empty-board [(Move. black-bishop :h1 :g2 nil)]))))
  (testing "white pawn moves from a2, but there is our pawn on a4"
    (is (= [(Move. white-pawn :a2 :a3 nil)] (white-pawn-possible-moves :a2 (place-piece empty-board :a4 white-pawn)
                                                                       [(Move. black-bishop :h1 :g2 nil)]))))
  (testing "white pawn moves from a2, but we can take black pawn on b3"
    (is (= [(Move. white-pawn :a2 :a3 nil) (Move. white-pawn :a2 :a4 nil) (Move. white-pawn :a2 :b3 nil)]
           (white-pawn-possible-moves :a2 (place-piece empty-board :b3 black-pawn) [(Move. black-bishop :h1 :g2 nil)])))))

(deftest black-pawn-captures-test
  (testing "black pawn captures from a7"
    (is (= [:b6] (black-pawn-captures :a7 (place-piece empty-board :b6 white-pawn))))))

(deftest black-pawn-possible-moves-test
  (testing "black pawn moves from a7"
    (is (= [(Move. black-pawn :a7 :a6 nil) (Move. black-pawn :a7 :a5 nil)]
           (black-pawn-possible-moves :a7 empty-board [(Move. white-bishop :h1 :g2 nil)]))))
  (testing "black pawn moves from a7, but there is our pawn on a5"
    (if [(Move. black-pawn :a7 :a6 nil)] (black-pawn-possible-moves :a7 (place-piece empty-board :a5 "♟") [(Move. white-bishop :h1 :g2 nil)]))))

(deftest rook-directions-test
  (testing "rook directions from d4"
    (is (= {:top [:d5 :d6 :d7 :d8] :downward [:d1 :d2 :d3] :left [:a4 :b4 :c4] :right [:e4 :f4 :g4 :h4]}
           (rook-directions :d4)))))

(deftest white-rook-possible-moves-test
  (testing "white rook possible moves from d4"
    (is (= [(Move. white-rook :d4 :d5 nil) (Move. white-rook :d4 :d6 nil) (Move. white-rook :d4 :d7 nil)
            (Move. white-rook :d4 :d8 nil) (Move. white-rook :d4 :d3 nil) (Move. white-rook :d4 :d2 nil) (Move. white-rook :d4 :d1 nil)
            (Move. white-rook :d4 :c4 nil) (Move. white-rook :d4 :b4 nil) (Move. white-rook :d4 :a4 nil) (Move. white-rook :d4 :e4 nil)
            (Move. white-rook :d4 :f4 nil) (Move. white-rook :d4 :g4 nil) (Move. white-rook :d4 :h4 nil)]
            (white-rook-possible-moves :d4 empty-board start-game-state start-moves-history))))
  (testing "white rook possible moves from d4 but there is white pawn on d6"
    (is (= [(Move. white-rook :d4 :d5 nil) (Move. white-rook :d4 :d3 nil) (Move. white-rook :d4 :d2 nil) (Move. white-rook :d4 :d1 nil)
            (Move. white-rook :d4 :c4 nil) (Move. white-rook :d4 :b4 nil) (Move. white-rook :d4 :a4 nil) (Move. white-rook :d4 :e4 nil)
            (Move. white-rook :d4 :f4 nil) (Move. white-rook :d4 :g4 nil) (Move. white-rook :d4 :h4 nil)]
           (white-rook-possible-moves :d4 (place-piece empty-board :d6 white-pawn) start-game-state start-moves-history))))
  (testing "white rook possible moves from d4, but there is white pawn on b4"
    (is (= [(Move. white-rook :d4 :d5 nil) (Move. white-rook :d4 :d6 nil) (Move. white-rook :d4 :d7 nil) (Move. white-rook :d4 :d8 nil)
            (Move. white-rook :d4 :d3 nil) (Move. white-rook :d4 :d2 nil) (Move. white-rook :d4 :d1 nil) (Move. white-rook :d4 :c4 nil)
            (Move. white-rook :d4 :e4 nil) (Move. white-rook :d4 :f4 nil)
            (Move. white-rook :d4 :g4 nil) (Move. white-rook :d4 :h4 nil)]
           (white-rook-possible-moves :d4 (place-piece empty-board :b4 white-pawn) start-game-state start-moves-history))))
  (testing "white rook possible moves from d4, but there is black pawn on b4"
    (is (= [(Move. white-rook :d4 :d5 nil) (Move. white-rook :d4 :d6 nil) (Move. white-rook :d4 :d7 nil) (Move. white-rook :d4 :d8 nil)
            (Move. white-rook :d4 :d3 nil) (Move. white-rook :d4 :d2 nil) (Move. white-rook :d4 :d1 nil) (Move. white-rook :d4 :c4 nil)
            (Move. white-rook :d4 :b4 nil) (Move. white-rook :d4 :e4 nil) (Move. white-rook :d4 :f4 nil) (Move. white-rook :d4 :g4 nil)
            (Move. white-rook :d4 :h4 nil)]
           (white-rook-possible-moves :d4 (place-piece empty-board :b4 black-pawn) start-game-state start-moves-history)))))

(deftest black-rook-possible-moves-test
  (testing "black rook possible moves from d4"
    (is (= [(Move. black-rook :d4 :d5 nil) (Move. black-rook :d4 :d6 nil) (Move. black-rook :d4 :d7 nil) (Move. black-rook :d4 :d8 nil)
            (Move. black-rook :d4 :d3 nil) (Move. black-rook :d4 :d2 nil) (Move. black-rook :d4 :d1 nil) (Move. black-rook :d4 :c4 nil)
            (Move. black-rook :d4 :b4 nil) (Move. black-rook :d4 :a4 nil) (Move. black-rook :d4 :e4 nil) (Move. black-rook :d4 :f4 nil)
            (Move. black-rook :d4 :g4 nil) (Move. black-rook :d4 :h4 nil)]
            (black-rook-possible-moves :d4 empty-board start-game-state start-moves-history))))
  (testing "black rook possible moves from d4 but there is white pawn on d6"
    (is (= [(Move. black-rook :d4 :d5 nil) (Move. black-rook :d4 :d6 nil) (Move. black-rook :d4 :d3 nil) (Move. black-rook :d4 :d2 nil)
            (Move. black-rook :d4 :d1 nil) (Move. black-rook :d4 :c4 nil) (Move. black-rook :d4 :b4 nil) (Move. black-rook :d4 :a4 nil)
            (Move. black-rook :d4 :e4 nil) (Move. black-rook :d4 :f4 nil) (Move. black-rook :d4 :g4 nil)
            (Move. black-rook :d4 :h4 nil)]
           (black-rook-possible-moves :d4 (place-piece empty-board :d6 white-pawn) start-game-state start-moves-history))))
  (testing "black rook possible moves from d4 but there is black pawn on d6"
    (is (= [(Move. black-rook :d4 :d5 nil)  (Move. black-rook :d4 :d3 nil) (Move. black-rook :d4 :d2 nil)
            (Move. black-rook :d4 :d1 nil) (Move. black-rook :d4 :c4 nil) (Move. black-rook :d4 :b4 nil) (Move. black-rook :d4 :a4 nil)
            (Move. black-rook :d4 :e4 nil) (Move. black-rook :d4 :f4 nil) (Move. black-rook :d4 :g4 nil)
            (Move. black-rook :d4 :h4 nil)]
           (black-rook-possible-moves :d4 (place-piece empty-board :d6 black-pawn) start-game-state start-moves-history)))))

(deftest bishop-directions-test
  (testing "bishop directions"
    (is (= {:top-right [:e5 :f6 :g7 :h8] :top-left [:a7 :b6 :c5] :down-right [:e3 :f2 :g1] :down-left [:a1 :b2 :c3]}
           (bishop-directions :d4)))))

(deftest bishop-possible-moves-test
  (testing "white bishop possible moves from d4, but there is a black pawn on g7"
    (is (= [(Move. white-bishop :d4 :e5 nil) (Move. white-bishop :d4 :f6 nil) (Move. white-bishop :d4 :g7 nil) (Move. white-bishop :d4 :c5 nil)
            (Move. white-bishop :d4 :b6 nil) (Move. white-bishop :d4 :a7 nil) (Move. white-bishop :d4 :e3 nil) (Move. white-bishop :d4 :f2 nil)
            (Move. white-bishop :d4 :g1 nil) (Move. white-bishop :d4 :c3 nil) (Move. white-bishop :d4 :b2 nil)
            (Move. white-bishop :d4 :a1 nil)]
           (bishop-possible-moves :d4 (place-piece empty-board :g7 black-pawn) white start-game-state start-moves-history))))
  (testing "white bishop possible moves from a1, but there is our pawn on d4"
    (is (= [(Move. white-bishop :a1 :b2 nil) (Move. white-bishop :a1 :c3 nil)]
           (bishop-possible-moves :a1 (place-piece empty-board :d4 white-pawn) white start-game-state start-moves-history))))
  (testing "white bishop possible moves from f1, but there is a white pawn on e2"
    (is (= [(Move. white-bishop :f1 :g2 nil) (Move. white-bishop :f1 :h3 nil)]
           (bishop-possible-moves :f1 (place-piece empty-board :e2 white-pawn) white start-game-state start-moves-history)))))

(deftest knight-possible-moves-test
  (testing "white knight moves from d4, but there is a white pawn on c2"
    (is (= [(Move. white-knight :d4 :b3 nil) (Move. white-knight :d4 :b5 nil) (Move. white-knight :d4 :c6 nil) (Move. white-knight :d4 :e2 nil)
             (Move. white-knight :d4 :e6 nil) (Move. white-knight :d4 :f3 nil) (Move. white-knight :d4 :f5 nil)]
           (knight-possible-moves :d4 (place-piece empty-board :c2 white-pawn) white start-game-state start-moves-history))))
  (testing "knight possible-moves from a1"
    (is (= [(Move. white-knight :a1 :b3 nil) (Move. white-knight :a1 :c2 nil)]
           (knight-possible-moves :a1 empty-board white start-game-state start-moves-history)))))

(deftest queen-possible-moves-test
  (testing "white queen possible moves from a1"
    (is (= [(Move. white-queen :a1 :a2 nil) (Move. white-queen :a1 :a3 nil) (Move. white-queen :a1 :a4 nil) (Move. white-queen :a1 :a5 nil)
            (Move. white-queen :a1 :a6 nil) (Move. white-queen :a1 :a7 nil) (Move. white-queen :a1 :a8 nil) (Move. white-queen :a1 :b1 nil)
            (Move. white-queen :a1 :c1 nil) (Move. white-queen :a1 :d1 nil) (Move. white-queen :a1 :e1 nil) (Move. white-queen :a1 :f1 nil)
            (Move. white-queen :a1 :g1 nil) (Move. white-queen :a1 :h1 nil) (Move. white-queen :a1 :b2 nil) (Move. white-queen :a1 :c3 nil)
            (Move. white-queen :a1 :d4 nil) (Move. white-queen :a1 :e5 nil) (Move. white-queen :a1 :f6 nil) (Move. white-queen :a1 :g7 nil)
            (Move. white-queen :a1 :h8 nil)]
           (queen-possible-moves :a1 empty-board white start-game-state start-moves-history))))
  (testing "white queen possible moves, but there is our pawn on d4"
    (is (= [(Move. white-queen :a1 :a2 nil) (Move. white-queen :a1 :a3 nil) (Move. white-queen :a1 :a4 nil) (Move. white-queen :a1 :a5 nil)
            (Move. white-queen :a1 :a6 nil) (Move. white-queen :a1 :a7 nil) (Move. white-queen :a1 :a8 nil) (Move. white-queen :a1 :b1 nil)
            (Move. white-queen :a1 :c1 nil) (Move. white-queen :a1 :d1 nil) (Move. white-queen :a1 :e1 nil) (Move. white-queen :a1 :f1 nil)
            (Move. white-queen :a1 :g1 nil) (Move. white-queen :a1 :h1 nil) (Move. white-queen :a1 :b2 nil) (Move. white-queen :a1 :c3 nil)]
           (queen-possible-moves :a1 (place-piece empty-board :d4 white-pawn) white start-game-state start-moves-history)))))

(deftest possible-moves-test
  (testing "possible moves test"
    (is (= [
            (Move. white-king :a1 :a2 nil)
            (Move. white-king :a1 :b1 nil )
            (Move. white-king :a1 :b2 nil)]
           (possible-moves :a1 (place-piece empty-board :a1 white-king) white start-game-state start-moves-history)))))

(deftest pieces-captures-test
  (testing "pawn captures"
    (is (= [:d5]
           (pieces-captures (place-piece
                              (place-piece empty-board :e4 white-pawn) :d5 black-pawn) white false start-game-state start-moves-history))))
  (testing "knight captures"
    (is (= [:b3 :c2]
           (pieces-captures
             (place-piece (place-piece empty-board :a1 white-knight) :c2 black-pawn) white false start-game-state start-moves-history)))))

(deftest king-possible-moves-test
  (testing "king possible moves, but there is our rook on a2"
    (is (= [(Move. white-king :a1 :b1 nil) (Move. white-king :a1 :b2 nil)]
           (king-possible-moves :a1 (place-piece empty-board :a2 white-rook) white start-game-state start-moves-history))))
  (testing "inf loop"
    (is (= [(Move. white-king :e1 :e2 nil)] (king-possible-moves
                   :e1 (move-piece initial-board (Move. white-pawn :e2 :e4 nil)) white start-game-state start-moves-history)))))

(deftest white-castling-test
  (testing "can white castling"
    (is (= (assoc (assoc empty-board :g1 white-king) :f1 white-rook)
           (white-castling (Move. white-king :e1 :g1 nil)
                           (assoc (assoc empty-board :e1 white-king) :h1 white-rook)
                           start-game-state start-moves-history)))))

(deftest black-castling-test
  (testing "can black castling"
    (is (= (assoc (assoc empty-board :g8 black-king) :f8 black-rook)
           (black-castling (Move. black-king :e8 :g8 nil)
                           (assoc (assoc empty-board :e8 black-king) :h8 black-rook)
                           start-game-state start-moves-history)))))

(deftest initial-board-test
  (testing "squares count"
    (is (= 64 (count initial-board))))
  (testing "what is on a1"
    (is (= white-rook (:a1 initial-board)))))

(deftest check?-test
  (testing "check by queen"
    (is (check? (place-piece (place-piece empty-board :a1 white-king) :a8 black-queen) white start-game-state start-moves-history)))
  (testing "check by black knight"
    (is (check? (place-piece (place-piece empty-board :a1 white-king) :c2 black-knight) white start-game-state start-moves-history)))
  (testing "check by white knight"
    (is (check? (place-piece (place-piece empty-board :a1 black-king) :c2 white-knight) black start-game-state start-moves-history)))
  (testing "check by rook"
    (is (check? (place-piece (place-piece empty-board :a1 white-king) :a8 black-rook) white start-game-state start-moves-history)))
  (testing "check by bishop"
    (is (check? (place-piece (place-piece empty-board :a1 white-king) :h8 black-bishop) white start-game-state start-moves-history)))
  (testing "check by pawn"
    (is (check? (place-piece (place-piece empty-board :a1 white-king) :b2 black-pawn) white start-game-state start-moves-history)))
  (testing "inf loop on initial position"
    (is (not (check? (place-piece (place-piece initial-board :e4 white-pawn) :e2 nil) white start-game-state start-moves-history)))))

(deftest checkmate?-test
  (testing "checkmate by two black rooks"
    (is (checkmate?
          (place-piece
            (place-piece
              (place-piece empty-board :a1 white-king) :a8 black-rook) :b8 black-rook) white start-game-state start-moves-history)))
  (testing "just check"
    (is (not (checkmate? (place-piece
                           (place-piece
                             (place-piece
                               (place-piece empty-board :a1
                                            white-king) :a8
                               black-rook) :b8
                             black-rook)
                           :h2 white-rook) white start-game-state start-moves-history))))
  (testing "is not mate after Kb3"
    (is (not (checkmate?
               (move-piece initial-board (Move. white-knight :b1 :c3 nil)) black start-game-state [(Move. white-knight :b1 :c3 nil)])))))

#_(

    ♜  ♞  ♝     ♚     ♞  ♜
       ♟        ♟  ♝  ♟  ♟  ♟
       ♟        ♛
       ♟        ♟
       ♙  ♙
       ♙  ♘
       ♙  ♙  ♙  ♙           ♙
       ♖  ♘  ♗  ♕  ♔  ♗     ♖

       )

(deftest valid-move?-test
  (testing "move is valid"
    (is (valid-move? (Move. white-pawn :e2 :e4 nil) initial-board white start-game-state start-moves-history))))

(deftest make-move-test
  (testing "pawn e2-e4"
    (is (= [(place-piece (place-piece initial-board :e4 white-pawn) :e2 nil) start-game-state
            [(Move. white-pawn :e2 :e4 nil)] true]
           (make-move (Move. white-pawn :e2 :e4 nil) initial-board white start-game-state start-moves-history))))
  (testing "castling"
    (is (= [(assoc (assoc empty-board :g1 white-king) :f1 white-rook)
            {:white-kingside-castling false :white-queenside-castling false :black-kingside-castling true :black-queenside-castling true}
            [(Move. white-king :e1 :g1 nil)] true]
           (make-move (Move. white-king :e1 :g1 nil)
                      (assoc (assoc empty-board :e1 white-king) :h1 white-rook)
                      white start-game-state start-moves-history))))
  (testing "promotion"
    (is (= [(place-piece empty-board :a8 white-queen)
            start-game-state
            [(Move. white-pawn :a7 :a8 white-queen)]
            true]
           (make-move (Move. white-pawn :a7 :a8 white-queen)
                      (place-piece empty-board :a7 white-pawn)
                      white start-game-state start-moves-history))))
  (testing "white en-passant"
    (is (= [(place-piece empty-board :e6 white-pawn)
            start-game-state
            [(Move. black-pawn :e7 :e5 nil) (Move. white-pawn :f5 :e6 nil)] true]
           (make-move (Move. white-pawn :f5 :e6 nil)
                      (place-piece (place-piece empty-board :f5 white-pawn) :e5 black-pawn)
                      white start-game-state
                      [(Move. black-pawn :e7 :e5 nil)]))))
  (testing "black en-passant"
    (is (= [(place-piece empty-board :d3 black-pawn)
            start-game-state
            [(Move. white-pawn :d2 :d4 nil) (Move. black-pawn :c4 :d3 nil)] true]
           (make-move (Move. black-pawn :c4 :d3 nil)
                      (place-piece (place-piece empty-board :d4 white-pawn) :c4 black-pawn) black start-game-state
                      [(Move. white-pawn :d2 :d4 nil)])))))

#_(deftest white-castling?-test
  (testing "can we castling"
    (is (white-castling? (Move. white-king :e1 :g1 nil) empty-board start-game-state))))
