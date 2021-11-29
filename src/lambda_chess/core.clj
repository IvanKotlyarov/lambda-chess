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

(defn rook-possible-moves [square-name board color]
  (let [
        directions (rook-directions square-name)
        top (:top directions)
        downward (:downward directions)
        left (:left directions)
        right (:right directions)
        top-moves (sort (reduce #(if (empty? (%2 board)) (conj % %2) (reduced %)) [] top))
        top-moves (if (= 8 (row (last top-moves)))
                    top-moves
                    (if (= color (:color (:pieceColor ((nth top (inc (.indexOf top-moves (last top-moves)))) board))))
                      top-moves
                      (conj top-moves (nth top (inc (.indexOf top-moves (last top-moves)))))))

        downward-moves (sort (reduce #(if (empty? (%2 board)) (conj % %2) (reduced %)) [] downward))
        downward-moves (if (= 1 (row (first downward-moves)))
                         downward-moves
                         (if (= color (:color (:pieceColor ((nth downward (inc (.indexOf downward-moves (last downward-moves)))) board))))
                           downward-moves
                           (conj downward-moves (nth downward (inc (.indexOf downward-moves (last downward-moves)))))))

        left-moves (sort (reduce #(if (empty? (%2 board)) (conj % %2) (reduced %)) [] left))
        left-moves (if (= "a" (col (first left-moves)))
                     left-moves
                     (if (= color (:color (:pieceColor ((nth left (inc (.indexOf left-moves (last left-moves)))) board))))
                       left-moves
                       (conj left-moves (nth left (inc (.indexOf left-moves (last left-moves)))))))

        right-moves (sort (reduce #(if (empty? (%2 board)) (conj % %2) (reduced %)) [] right))
        right-moves (if (= "h" (col (last right-moves)))
                      right-moves
                      (if (= color (:color (:pieceColor ((nth right (inc (.indexOf right-moves (last right-moves)))) board))))
                        right-moves
                        (conj right-moves (nth right (inc (.indexOf right-moves (last right-moves)))))))
        ]
    (into [] (concat top-moves downward-moves left-moves right-moves))))

(defn white-rook-possible-moves [square-name board] (rook-possible-moves square-name board "white"))
(defn black-rook-possible-moves [square-name board] (rook-possible-moves square-name board "black"))

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

(defn bishop-possible-moves [square-name board color]
  (let [
        directions (bishop-directions square-name)
        top-right (:top-right directions)
        top-left (:top-left directions)
        down-right (:down-right directions)
        down-left (:down-left directions)
        top-right (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:color (:pieceColor (%2 board))))
                                                                 (reduced (conj % %2))
                                                                 (reduced %)))
                          [] top-right)

        top-left (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:color (:pieceColor (%2 board))))
                                                                (reduced (conj % %2))
                                                                (reduced %)))
                         [] top-left)

        down-right (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:color (:pieceColor (%2 board))))
                                                                  (reduced (conj % %2))
                                                                  (reduced %)))
                           [] down-right)

        down-left (reduce #(if (empty? (%2 board)) (conj % %2) (if (not= color (:color (:pieceColor (%2 board))))
                                                                 (reduced (conj % %2))
                                                                 (reduced %)))
                          [] down-left)
        ]
    (into [] (concat top-right top-left down-right down-left))))
