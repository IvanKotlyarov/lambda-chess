(ns lambda-chess.core
  (:require [clojure.math.combinatorics :as combo]))

(defrecord PieceColor [^String color])

(def white (PieceColor. "white"))
(def black (PieceColor. "black"))

(defrecord PieceType [^String type])

(def rook (PieceType. "rook"))
(def knight (PieceType. "knight"))
(def bishop (PieceType. "bishop"))
(def queen (PieceType. "queen"))
(def king (PieceType. "king"))
(def pawn (PieceType. "pawn"))

(defrecord Game [board moves game-state])

(defrecord Piece [^PieceType pieceType ^PieceColor pieceColor ^String unicode])

(def white-king   (Piece. king   white "♔"))
(def white-queen  (Piece. queen  white "♕"))
(def white-rook   (Piece. rook   white "♖"))
(def white-bishop (Piece. bishop white "♗"))
(def white-knight (Piece. knight white "♘"))
(def white-pawn   (Piece. pawn   white "♙"))

(def black-king   (Piece. king   black "♚"))
(def black-queen  (Piece. queen  black "♛"))
(def black-rook   (Piece. rook   black "♜"))
(def black-bishop (Piece. bishop black "♝"))
(def black-knight (Piece. knight black "♞"))
(def black-pawn   (Piece. pawn   black "♟"))

(defrecord Square [^Piece piece])

(defrecord Move [^Piece piece from to ^Piece promoted])

(def col-names ["a" "b" "c" "d" "e" "f" "g" "h"])
(def row-names [1 2 3 4 5 6 7 8])

(defn generate-square-names []
  (map (fn [[col row]] (keyword (str col row))) (combo/cartesian-product col-names row-names)))

(def square-names (generate-square-names))

(def start-game-state {:white-queenside-castling true
                       :white-kingside-castling true
                       :black-queenside-castling true
                       :black-kingside-castling true})

(defn make-game-state [previous key value]
  (assoc previous key value))

(def start-moves-history [])

(defn make-history [history move]
  (conj history move))

(defn make-moves [from to ^Piece piece prev promoted]
  (conj prev (Move. piece from to promoted)))

(defn abs [a]
  (max a (* -1 a)))

(defn other-color [^PieceColor color]
  (if (= white color) black white))

(defn generate-empty-board []
  (reduce #(assoc % %2 nil) {} square-names))

(def empty-board (generate-empty-board))

(def pieces {:a1 white-rook :b1 white-knight :c1 white-bishop :d1 white-queen
             :e1 white-king :f1 white-bishop :g1 white-knight :h1 white-rook
             :a8 black-rook :b8 black-knight :c8 black-bishop :d8 black-queen
             :e8 black-king :f8 black-bishop :g8 black-knight :h8 black-rook})

(defn pawns [^PieceColor color]
  (let [[r p] (if (= color white) [2 white-pawn] [7 black-pawn])]
    (reduce #(assoc % (keyword (str %2 r)) p) {} col-names)))

(def initial-board (merge empty-board (merge (pawns black) (pawns white) pieces)))

(defn place-piece [board square ^Piece piece]
  (assoc board square piece))

(defn move-piece [board move]
  (assoc (assoc board (:from move) nil) (:to move) (:piece move)))

(defn row [square-name]
  (Character/digit (get (str square-name) 2) 10))

(defn col [square-name]
  (str (get (str square-name) 1)))

(defn white-pawn-moves [square-name]
  (let [
        c (col square-name)
        r (row square-name)
        possible-moves [(keyword (str c (inc r)))]
        possible-moves (if (= r 2)
                         (conj possible-moves (keyword (str c (+ 2 r))))
                         possible-moves)]
    possible-moves))

(defn black-pawn-moves [square-name]
  (let [
        c (col square-name)
        r (row square-name)
        possible-moves [(keyword (str c (dec r)))]
        possible-moves (if (= r 7)
                         (conj possible-moves
                               (keyword (str c (- r 2))))
                         possible-moves)]
    possible-moves))

(defn rook-moves [square-name]
  (let [
        squarescol (filter #(if (and (= (col square-name) (col %)) (not= square-name %)) true false) square-names)
        squaresrow (filter #(if (and (= (row square-name) (row %)) (not= square-name %)) true false) square-names)
        possible-moves (into [] (concat squarescol squaresrow))]
    possible-moves))

(defn bishop-moves [square-name]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))
        possible-moves (filter #(and (=  (abs (- index-col (.indexOf col-names (col %))))
                                         (abs (- index-row (.indexOf row-names (row %)))))
                                     (not= square-name %)) square-names)]
  possible-moves))

(defn queen-moves [square-name]
  (into [] (concat (bishop-moves square-name) (rook-moves square-name))))

(defn king-moves [square-name]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))]
    (filter #(and
               (>= 1 (abs (- index-col (.indexOf col-names (col %)))))
               (>= 1 (abs (- index-row (.indexOf row-names (row %)))))
               (not= square-name %)) square-names)))

(defn knight-moves [square-name]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))
        possible-moves (filter #(or
                                  (and (= 1 (abs (- index-col (.indexOf col-names (col %)))))
                                       (= 2 (abs (- index-row (.indexOf row-names (row %))))))
                                  (and (= 2 (abs (- index-col (.indexOf col-names (col %)))))
                                       (= 1 (abs (- index-row (.indexOf row-names (row %)))))))
                               square-names)]
    possible-moves))

(defn white-pawn-captures [square-name board]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))
        captures (filter #(or
                            (and (= -1 (- index-col (.indexOf col-names (col %))))
                                 (= -1 (- index-row (.indexOf row-names (row %)))))
                            (and (= 1 (- index-col (.indexOf col-names (col %))))
                                 (= -1 (- index-row (.indexOf row-names (row %))))))
                     square-names)
        possible-captures (filter #(not= nil (% board)) captures)
        possible-captures (filter #(= black (:pieceColor (% board))) possible-captures)]
    possible-captures))

(defn black-pawn-captures [square-name board]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))
        captures (filter #(or
                            (and (= -1 (- index-col (.indexOf col-names (col %))))
                                 (= 1 (- index-row (.indexOf row-names (row %)))))
                            (and (= 1 (- index-col (.indexOf col-names (col %))))
                                 (= 1 (- index-row (.indexOf row-names (row %))))))
                         square-names)
        possible-captures (filter #(not= nil (% board)) captures)
        possible-captures (filter #(= white (:pieceColor (% board))) possible-captures)]
    possible-captures))

(defn pawn-captures [square-name board color]
  (if (= color black) (black-pawn-captures square-name board) (white-pawn-captures square-name board)))

(defn white-pawn-possible-moves [square-name board]
  (let [
        moves (white-pawn-moves square-name)
        possible-moves (if (= 2 (count moves))
                         (if (empty? ((first (sort moves)) board))
                           (if (empty? ((second (sort moves)) board))
                             moves
                             [(first moves)]) [])
                         (if (empty? ((first (sort moves)) board))
                           moves []))
        possible-moves (into [] (concat possible-moves (white-pawn-captures square-name board)))]
    possible-moves))

(defn black-pawn-possible-moves [square-name board]
  (let [
        moves (black-pawn-moves square-name)
        possible-moves (if (= 2 (count moves))
                         (if (empty? ((second (sort moves)) board))
                           (if (empty? ((first (sort moves)) board))
                             moves
                             [(first moves)])
                           [])
                         (if (empty? ((second (sort moves)) board))
                           moves
                           []))
        possible-moves (into [] (concat possible-moves (black-pawn-captures square-name board)))]
    possible-moves))

(defn pawn-possible-moves [square-name board ^PieceColor color]
  (if (= color white)
    (white-pawn-possible-moves square-name board)
    (black-pawn-possible-moves square-name board)))

(defn rook-directions [square-name]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))
        ; there is my direction below
        top-direction (filter #(and (= (col %) (col square-name))
                                    (< index-row (.indexOf row-names (row %))))
                              (rook-moves square-name))

        downward-direction (filter #(and (= (col %) (col square-name))
                                         (> index-row (.indexOf row-names (row %))))
                                   (rook-moves square-name))

        left-direction (filter #(and (= (row %) (row square-name))
                                     (> index-col (.indexOf col-names (col %))))
                               (rook-moves square-name))

        right-direction (filter #(and (= (row %) (row square-name))
                                      (< index-col (.indexOf col-names (col %))))
                                (rook-moves square-name))]
    {:top top-direction :downward downward-direction :left left-direction :right right-direction}))

(defn rook-possible-moves [square-name board ^PieceColor color]
  (let [
        directions (rook-directions square-name)
        top (:top directions)
        downward (reverse (sort (:downward directions)))
        left (reverse (sort (:left directions)))
        right (:right directions)
        top-moves (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:pieceColor (%2 board)))
                                                                 (reduced (conj % %2))
                                                                 (reduced %)))
                          [] top)

        downward-moves (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:pieceColor (%2 board)))
                                                                      (reduced (conj % %2))
                                                                      (reduced %)))
                               [] downward)

        left-moves (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:pieceColor (%2 board)))
                                                                  (reduced (conj % %2))
                                                                  (reduced %)))
                           [] left)

        right-moves (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:pieceColor (%2 board)))
                                                                   (reduced (conj % %2))
                                                                   (reduced %)))
                            [] right)
        ]
    (into [] (concat top-moves downward-moves left-moves right-moves))))

(defn white-rook-possible-moves [square-name board] (rook-possible-moves square-name board white))
(defn black-rook-possible-moves [square-name board] (rook-possible-moves square-name board black))

(defn bishop-directions [square-name]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))
        top-right (filter #(and (< index-col (.indexOf col-names (col %)))
                                (< index-row (.indexOf row-names (row %))))
                          (bishop-moves square-name))
        top-left (filter #(and (> index-col (.indexOf col-names (col %)))
                               (< index-row (.indexOf row-names (row %))))
                         (bishop-moves square-name))
        down-right (filter #(and (< index-col (.indexOf col-names (col %)))
                                 (> index-row (.indexOf row-names (row %))))
                           (bishop-moves square-name))
        down-left (filter #(and (> index-col (.indexOf col-names (col %)))
                                (> index-row (.indexOf row-names (row %))))
                          (bishop-moves square-name))]
    {:top-right top-right :top-left top-left :down-right down-right :down-left down-left}))

(defn bishop-possible-moves [square-name board ^PieceColor color]
  (let [
        directions (bishop-directions square-name)
        top-right (:top-right directions)
        top-left (reverse (:top-left directions))
        down-right (:down-right directions)
        down-left (reverse (:down-left directions))
        top-right (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:pieceColor (%2 board)))
                                                                 (reduced (conj % %2))
                                                                 (reduced %)))
                          [] top-right)

        top-left (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:pieceColor (%2 board)))
                                                                (reduced (conj % %2))
                                                                (reduced %)))
                         [] top-left)

        down-right (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:pieceColor (%2 board)))
                                                                  (reduced (conj % %2))
                                                                  (reduced %)))
                           [] down-right)

        down-left (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:pieceColor (%2 board)))
                                                                 (reduced (conj % %2))
                                                                 (reduced %)))
                          [] down-left)]
    (into [] (concat top-right top-left down-right down-left))))

(defn knight-possible-moves [square-name board ^PieceColor color]
  (filter #(not= color (:pieceColor (% board))) (knight-moves square-name)))

(defn queen-possible-moves [square-name board ^PieceColor color]
  (into [] (concat (rook-possible-moves square-name board color) (bishop-possible-moves square-name board color))))

(defn pieces-squares [board ^PieceColor color]
  (filter #(and (not (empty? (% board)))
                (= color (:pieceColor (% board))))
          (keys board)))

(defn possible-moves-squares [square-name board ^PieceColor color]
  (let [
        piece (str (:type (:pieceType (square-name board))))]
    ((resolve (symbol (str "lambda-chess.core/" piece "-possible-moves"))) square-name board color)))

(defn possible-moves [square-name board ^PieceColor color]
  (map #(Move. (square-name board) square-name % nil) (possible-moves-squares square-name board color)))

(defn pieces-captures [board ^PieceColor color ^Boolean except-king]
  (let [
        squares (pieces-squares board color)
        captures (set (flatten (reduce #(conj % (if (= (:pieceType (%2 board)) pawn)
                                                  (pawn-captures %2 board color)
                                                  (if (and except-king (= (:pieceType (%2 board)) king))
                                                    []
                                                    (possible-moves-squares %2 board color)))) [] squares)))]
    captures))

(defn king-possible-moves [square-name board ^PieceColor color]
  (let [captures (pieces-captures board (other-color color) true)]
    (filter #(and
               (not= color (:pieceColor (% board)))
               (not (contains? captures %)))
            (king-moves square-name))))

(defn white-castling [move board game-state]
  (let [
        captures (pieces-captures board black false)
        moves (if (= 3 (count (filter #(and (not (contains? captures %))
                                            (not= white (:pieceColor (% board)))
                                            (:white-queenside-castling game-state)) [:b1 :c1 :d1])))
                [:c1])
        moves (if (= 2 (count (filter #(and (not (contains? captures %))
                                            (:white-kingside-castling game-state)
                                            (not= white (:pieceColor (% board))))
                                      [:f1 :g1])))
                (conj moves :g1))
        [rook-square-from rook-square-to] (if (= :c1 (:to move)) [:a1 :d1] [:h1 :f1])
        new-board (if (some #(= % (:to move)) moves)
                    (move-piece (move-piece board move) (Move. white-rook rook-square-from rook-square-to nil)) nil)
        ]
    new-board))

(defn black-castling [move board game-state]
  (let [
        captures (pieces-captures board white false)

        moves (if (= 3 (count (filter #(and (not (contains? captures %))
                                            (not= black (:pieceColor (% board)))
                                            (:black-queenside-castling game-state)) [:b8 :c8 :d8])))
                [:c8])
        moves (if (= 2 (count (filter #(and (not (contains? captures %))
                                            (:black-kingside-castling game-state)
                                            (not= black (:pieceColor (% board))))
                                      [:f8 :g8])))
                (conj moves :g8))

        [rook-square-from rook-square-to] (if (= :c8 (:to move)) [:a8 :d8] [:h8 :f8])
        new-board (if (some #(= % (:to move)) moves)
                    (move-piece (move-piece board move) (Move. black-rook rook-square-from rook-square-to nil)) nil)
        ]
    new-board))

(defn en-passant-check [^Move move board history]
  (let [
        ; FIXME: check color to find out en-passant in above or below
        square-en-passant (keyword (str (col (:to move)) (dec (row (:to move)))))
        index-from (.indexOf col-names (col (:from move)))
        index-to (.indexOf col-names (col (:to move)))
        delta-index (abs (- index-from index-to))
        last-move (last history)
        ]
    (if (and (= pawn (:pieceType (:piece move))) (not= 0 delta-index) (empty? ((:to move) board)))
      (if (and (= pawn (:pieceType (square-en-passant board))) (= 2 (- (row (:from last-move)) (row (:to last-move)))))
        [move]
        [])
      [])))

(defn castling [move board game-state ^PieceColor color]
  (if (= color white) (white-castling move board game-state) (black-castling move board game-state)))

(defn pawn-promotion [square-name board piece]
  (assoc board square-name piece))

(defn all-possible-moves [^PieceColor color board game-state]
  (let [
        squares (pieces-squares board color)
        pieces-possible-moves (map #(possible-moves % board color) squares)
        ]
    (flatten pieces-possible-moves)))

(defn check? [board ^PieceColor color]
  (let [
        king-square (filter #(= king (:pieceType (% board))) (pieces-squares board color))
        captures (pieces-captures board (other-color color) false)]
    (contains? captures (first king-square))))

(defn checkmate [board ^PieceColor color]
  (let [
        our-pieces (pieces-squares board color)
        our-pieces-moves (set (flatten (reduce #(conj % (possible-moves %2 board color)) [] our-pieces)))
        moves (filter #(not (check? (assoc (assoc board (:from %) nil) (:to %) (:piece %)) color)) our-pieces-moves)
        ]
    moves))

(defn valid-move? [^Move move board color]
  (not (check? (assoc (assoc board (:from move) nil) (:to move) (:piece move)) color)))

(defn make-move [^Move move board color game-state history]
  (let [
        index-from (.indexOf col-names (col (:from move)))
        index-to (.indexOf col-names (col (:to move)))
        delta-index (abs (- index-from index-to))
        last-move (last history)
        square-en-passant (keyword (str (col (:to move)) (dec (row (:to move)))))
        new-game-state (if (and (= king (:pieceType (:piece move))) (not= 1 delta-index))
                         (assoc (assoc game-state (keyword (str (:color color) "-queenside-castling")) false)
                           (keyword (str (:color color) "-kingside-castling")) false)
                         game-state)

        [new-board comment] (if (valid-move? move board color)
                    (if (not= (:promoted move) nil)
                      (if (and
                            (= pawn (:pieceType (:piece move)))
                            (= 8 (row (:to move))) (= 7 (row (:from move)))
                            (not= pawn (:pieceType (:promoted move)))
                            (not= king (:pieceType (:promoted move))))
                        [(assoc (assoc board (:from move) nil) (:to move) (:promoted move)) true]
                        [board false])
                      (if (and (= king (:pieceType (:piece move))) (not= 1 delta-index))
                        [(castling move board game-state color) true]
                        (if (and (= pawn (:pieceType (:piece move))) (not= 0 delta-index) (empty? ((:to move) board)))
                          (if (and (= pawn (:pieceType (square-en-passant board))) (= 2 (- (row (:from last-move)) (row (:to last-move)))))
                            [(move-piece (assoc board square-en-passant nil) move) true]
                            [board false])
                          [(move-piece board move) true])))
                    [board false])
        new-history (make-history history move)]
    [new-board new-game-state new-history comment]))

(defn col-names-for-display [row-name]
  (reduce #(conj % (keyword (str %2 row-name))) [] col-names))

(defn square-names-for-display []
  (reduce #(conj % (col-names-for-display %2)) [] (reverse row-names)))

(defn print-board [board]
  (doseq [row (square-names-for-display)]
    (doseq [col row
            unicode (or (:unicode (col board)) " ")]
      (print unicode " "))
    (println)))

(defn clear-screen []
  (println (str (char 27) "[2J"))
  (println (str (char 27) "[;H")))

(defn make-random-move [board ^PieceColor color game-state history]
  (if (= [] (checkmate board color))
    :checkmate
    (do
      (clear-screen)
      (print-board board)
      #_(Thread/sleep 100)
      (let [ move (rand-nth (all-possible-moves color board game-state))
             [new-board new-game-state new-moves-history comment] (make-move move board color game-state history)]
        (recur new-board (other-color color) new-game-state new-moves-history)))))

(defn random-agent []
  (make-random-move initial-board white start-game-state start-moves-history))

