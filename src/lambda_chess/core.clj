(ns lambda-chess.core
  (:require [clojure.math.combinatorics :as combo]))

(defrecord PieceColor [color])

(def white (PieceColor. "white"))
(def black (PieceColor. "black"))

(defrecord PieceType [type])

(def rook (PieceType. "rook"))
(def knight (PieceType. "knight"))
(def bishop (PieceType. "bishop"))
(def queen (PieceType. "queen"))
(def king (PieceType. "king"))
(def pawn (PieceType. "pawn"))

(defrecord Piece [^PieceType pieceType ^PieceColor pieceColor ^String unicode])

(defrecord Square [^Piece piece])

(def col-names ["a" "b" "c" "d" "e" "f" "g" "h"])
(def row-names [1 2 3 4 5 6 7 8])
(defn generate-square-names []
  (map
    (fn [[col row]] (keyword (str col row))) (combo/cartesian-product col-names row-names)))
(def square-names (generate-square-names))

(def start-game-state {:white-queen-side-castling true
                       :white-king-side-castling true
                       :black-queen-side-castling true
                       :black-king-side-castling true})

(defn make-game-state [previous key value]
  (assoc previous key value))

(defn abs [a]
  (max a (* -1 a)))

(defn other-color [^PieceColor color]
  (if (= white color)
    black
    white))

(defn generate-empty-board []
  (reduce (fn [board square-name] (assoc board square-name nil)) {} square-names))

(def empty-board (generate-empty-board))

(defn place-piece [board square piece]
  (assoc board square piece))

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
        possible-moves []
        squarescol (filter #(if (and (= (col square-name) (col %)) (not= square-name %)) true false) square-names)
        squaresrow (filter #(if (and (= (row square-name) (row %)) (not= square-name %)) true false) square-names)
        possible-moves (into [] (concat possible-moves squarescol))
        possible-moves (into [] (concat possible-moves squaresrow))]
    possible-moves))

(defn bishop-moves [square-name]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))
        possible-moves (filter #(if (and (=  (abs (- index-col (.indexOf col-names (col %))))
                                             (abs (- index-row (.indexOf row-names (row %))))
                                             )
                                         (not= square-name %)) true false) square-names)]
  possible-moves))

(defn queen-moves [square-name]
  (into [] (concat (bishop-moves square-name) (rook-moves square-name)))
  )

(defn king-moves [square-name]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))
        possible-moves (filter #(if (and
                                      (>= 1 (abs (- index-col (.indexOf col-names (col %)))))
                                      (>= 1 (abs (- index-row (.indexOf row-names (row %)))))
                                      (not= square-name %)) true false) square-names)]
    possible-moves))

(defn knight-moves [square-name]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))
        possible-moves (filter #(if (or
                                      (and (= 1 (abs (- index-col (.indexOf col-names (col %)))))
                                           (= 2 (abs (- index-row (.indexOf row-names (row %))))))
                                      (and (= 2 (abs (- index-col (.indexOf col-names (col %)))))
                                           (= 1 (abs (- index-row (.indexOf row-names (row %)))))))
                                  true false)
                               square-names)]
    possible-moves))

(defn white-pawn-captures [square-name board]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))
        captures (filter #(if (or
                            (and (= -1 (- index-col (.indexOf col-names (col %))))
                                 (= -1 (- index-row (.indexOf row-names (row %)))))
                            (and (= 1 (- index-col (.indexOf col-names (col %))))
                                 (= -1 (- index-row (.indexOf row-names (row %)))))) true false)
                     square-names)
        possible-captures (filter #(if (= nil (% board)) false true) captures)
        possible-captures (filter #(if (= "black" (:color (:pieceColor (% board)))) true false) possible-captures)
        ]
    possible-captures))

(defn black-pawn-captures [square-name board]
  (let [
        index-col (.indexOf col-names (col square-name))
        index-row (.indexOf row-names (row square-name))
        captures (filter #(if (or
                                (and (= -1 (- index-col (.indexOf col-names (col %))))
                                     (= 1 (- index-row (.indexOf row-names (row %)))))
                                (and (= 1 (- index-col (.indexOf col-names (col %))))
                                     (= 1 (- index-row (.indexOf row-names (row %)))))) true false)
                         square-names)
        possible-captures (filter #(if (= nil (% board)) false true) captures)
        possible-captures (filter #(if (= "white" (:color (:pieceColor (% board)))) true false) possible-captures)
        ]
    possible-captures))

(defn white-pawn-possible-moves [square-name board]
  (let [
        moves (white-pawn-moves square-name)
        possible-moves (if (= 2 (count moves))
                         (if (empty? ((first (sort moves)) board))
                           (if (empty? ((second (sort moves)) board))
                             moves
                             [(first moves)])
                           [])
                         (if (empty? ((first (sort moves)) board))
                           moves
                           []))
        possible-moves (into [] (concat possible-moves (white-pawn-captures square-name board)))
        ]
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
        possible-moves (into [] (concat possible-moves (black-pawn-captures square-name board)))
        ]
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
                                (rook-moves square-name))
        ]
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
                          (bishop-moves square-name))

        ]
    {:top-right top-right :top-left top-left :down-right down-right :down-left down-left}))

(defn bishop-possible-moves [square-name board ^PieceColor color]
  (let [
        directions (bishop-directions square-name)
        top-right (:top-right directions)
        top-left (:top-left directions)
        down-right (:down-right directions)
        down-left (:down-left directions)

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
                          [] down-left)
        ]
    (into [] (concat top-right top-left down-right down-left))))

(defn queen-possible-moves [square-name board color]
  (into [] (concat (rook-possible-moves square-name board color) (bishop-possible-moves square-name board color))))

(defn other-pieces-squares [board ^PieceColor color]
  (filter #(and (not (empty? (% board)))
                (= color (:pieceColor (% board))))
          (keys board)))

(defn possible-moves [square-name board ^PieceColor color]
  (let [
        piece (str (:type (:pieceType (square-name board))))]

    ((resolve (symbol (str "lambda-chess.core/" piece "-possible-moves"))) square-name board color)))

(defn king-possible-moves [square-name board color]
  (let [
        other-pieces (other-pieces-squares board (other-color color))
        other-pieces-captures (set (flatten (reduce #(conj % (possible-moves %2 board (other-color color))) [] other-pieces)))
        moves (filter #(if (and (not (contains? other-pieces-captures %))
                                (or (empty? (% board))
                                  (not= color (:pieceColor (% board)))))
                         true
                         false)
                      (king-moves square-name))
        ]
    moves)
  )
(defn white-castling [board game-state]
  (let [
        other-pieces (other-pieces-squares board black)
        other-pieces-captures (set (flatten (reduce #(conj % (possible-moves %2 board black)) [other-pieces])))

        moves (if (= 3 (count (filter #(and (not (contains? other-pieces-captures %))
                                            (not= white (:pieceColor (% board)))
                                            (:white-queen-side-castling game-state)) [:b1 :c1 :d1])))
                [:c1])
        moves (if (= 2 (count (filter #(and (not (contains? other-pieces-captures %))
                                            (not= white (:pieceColor (% board))))
                                      [:f1 :g1])))
                (conj moves :g1))]
    moves))

(defn black-castling [board game-state]
  (let [
        other-pieces (other-pieces-squares board white)
        other-pieces-captures (set (flatten (reduce #(conj % (possible-moves %2 white black)) [other-pieces])))

        moves (if (= 3 (count (filter #(and (not (contains? other-pieces-captures %))
                                            (not= black (:pieceColor (% board)))
                                            (:white-queen-side-castling game-state)) [:b8 :c8 :d8])))
                [:c8])
        moves (if (= 2 (count (filter #(and (not (contains? other-pieces-captures %))
                                            (not= black (:pieceColor (% board))))
                                      [:f8 :g8])))
                (conj moves :g8))]
    moves))

