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

(defn abs [a]
  (max a (* -1 a)))

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
        possible-moves []
        col (str (get (str square-name) 1))
        row (Character/digit (get (str square-name) 2) 10)
        possible-moves (if (= row 2)
            (conj possible-moves
                              (keyword (str col (inc row)))
                              (keyword (str col (+ 2 row))))
            (conj possible-moves (keyword (str col (inc row)))))]
    possible-moves))

(defn black-pawn-moves [square-name]
  (let [
        possible-moves []
        col (str (get (str square-name) 1))
        row (Character/digit (get (str square-name) 2) 10)
        possible-moves (if (= row 7)
                         (conj possible-moves
                               (keyword (str col (dec row)))
                               (keyword (str col (- row 2))))
                         (conj possible-moves (keyword (str col (dec row)))))]
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