{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Core where

import           Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Map as M
import Debug.Trace
import Data.Foldable
import Data.Function
import Data.List (intercalate, delete, sortOn)
import Data.Char
import Data.Text (Text, pack, splitOn, unpack)

-- ADT - Algebraic Data-Type 
-- data <Конструктор типа> = <Конструкторы данных>

-- Тип сумма:
data Color = White | Black
  deriving (Eq, Show)

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
  deriving (Eq, Show)

-- Тип произведения:
data Piece = Piece PieceType Color
  deriving (Eq, Show)

type Square = (Char, Int)

cols :: [Char]
cols = ['a' .. 'h']

rows :: [Int]
rows = [1 .. 8]

data Board = Board (M.Map Square Piece) (Maybe Square) Bool Bool Bool Bool [Move] Int Color
  deriving (Eq, Show)

findPiece :: Board -> Square -> Maybe Piece
findPiece (Board squares _ _ _ _ _ _ _ _) square = M.lookup square squares

squares :: Board -> M.Map Square Piece
squares board@(Board sqs _ _ _ _ _ _ _ _) = sqs
-- d8=Q   -- простое продвижение
-- cxd8=Q -- взятие с последующим продвижением

data Move
    = Move Piece Square Square
    | DoubleSquare Square Square
    | Capture Piece Square Square
    | Promotion Square Square Piece
    | CapturePromotion Square Square Piece
    | EnPassant Piece Square Square
    | KingsideCastling Color
    | QueensideCastling Color
    deriving (Eq, Show)

toSquare :: Move -> Square
toSquare move@(Move _ _ s) = s
toSquare move@(DoubleSquare _ s) = s
toSquare move@(Capture _ _ s) = s
toSquare move@(Promotion _ s _) = s
toSquare move@(CapturePromotion _ s _) = s
toSquare move@(EnPassant _ _ s) = s
toSquare move = undefined

other :: Color -> Color
other White = Black
other _     = White

whitePawn, whiteKnight, whiteBishop, whiteRook, whiteQueen, whiteKing :: Piece
whitePawn = Piece Pawn White
whiteKnight = Piece Knight White
whiteBishop = Piece Bishop White
whiteRook = Piece Rook White
whiteQueen = Piece Queen White
whiteKing = Piece King White

blackPawn, blackKnight, blackBishop, blackRook, blackQueen, blackKing :: Piece
blackPawn = Piece Pawn Black
blackKnight = Piece Knight Black
blackBishop = Piece Bishop Black
blackRook = Piece Rook Black
blackQueen = Piece Queen Black
blackKing = Piece King Black

emptyBoard :: Board
emptyBoard = Board M.empty Nothing False False False False [] 0 White

initialBoard :: Board
initialBoard = Board (M.fromList (whitePawns ++ pieces White ++ blackPawns ++ pieces Black)) Nothing True True True True [] 0 White
    where
        whitePawns = map (\c -> ((c, 2), whitePawn)) cols
        blackPawns = map (\c -> ((c, 7), blackPawn)) cols
        row color = if color == White then 1 else 8
        pieces :: Color -> [(Square, Piece)]
        pieces color = zipWith (\c p -> ((c, row color), p)) cols
                [ Piece Rook color, Piece Knight color, Piece Bishop color, Piece Queen color
                , Piece King color, Piece Bishop color, Piece Knight color, Piece Rook color
                ]

history :: Board -> [Move]
history (Board _ _ _ _ _ _ h _ _) = h

isOnBoard :: Square -> Bool
isOnBoard (col, row)
  -- col `elem` cols && row `elem` rows
  -- a bit faster:
  = col >= 'a' && col <= 'h' && row >= 1 && row <= 8

placePiece :: Square -> Piece -> Board -> Board
placePiece square piece (Board squares enPassant wkc wqc bkc bqc h cnt c) = Board (M.insert square piece squares) enPassant wkc wqc bkc bqc h cnt c

deletePiece :: Square -> Board -> Board
deletePiece square (Board squares enPassant wkc wqc bkc bqc h cnt c) = Board (M.delete square squares) enPassant wkc wqc bkc bqc h cnt c

movePiece :: Move -> Board -> Board
movePiece m@(KingsideCastling White) (Board sqs _ _ _ kingSide queenSide h cnt _)
    = placePiece ('f', 1) (Piece Rook White)
    $ deletePiece ('h', 1)
    $ placePiece ('g', 1) (Piece King White)
    $ deletePiece ('e', 1) (Board sqs Nothing False False kingSide queenSide (h ++ [m]) (cnt + 1) Black)

movePiece m@(KingsideCastling Black) (Board sqs _ kingSide queenSide _ _ h cnt _)
    = placePiece ('f', 8) (Piece Rook Black)
    $ deletePiece ('h', 8)
    $ placePiece ('g', 8) (Piece King Black)
    $ deletePiece ('e', 8) (Board sqs Nothing kingSide queenSide False False (h ++ [m]) (cnt + 1) White)

movePiece m@(QueensideCastling White) (Board sqs _ _ _ kingSide queenSide h cnt _)
    = placePiece ('d', 1) (Piece Rook White)
    $ deletePiece ('a', 1)
    $ placePiece ('c', 1) (Piece King White)
    $ deletePiece ('e', 1) (Board sqs Nothing False False kingSide queenSide (h ++ [m]) (cnt + 1) Black)

movePiece m@(QueensideCastling Black) (Board sqs _ kingSide queenSide _ _ h cnt _)
    = placePiece ('d', 8) (Piece Rook Black)
    $ deletePiece ('a', 8)
    $ placePiece ('c', 8) (Piece King Black)
    $ deletePiece ('e', 8) (Board sqs Nothing kingSide queenSide False False (h ++ [m]) (cnt + 1) White)

movePiece m@(DoubleSquare from@(fromCol, fromRow) to@(toCol, _)) board@(Board _ _ wk wq bk bq h _ c)
    = Board sqs (Just enPassant) wk wq bk bq (h ++ [m]) 0 (other c)
    where
        sqs = squares $ placePiece to (if fromRow == 2 then whitePawn else blackPawn)
            $ deletePiece from
            $ deletePiece (toCol, fromRow) board
        enPassant = if fromRow == 2 then (fromCol, succ fromRow) else (fromCol, pred fromRow)

movePiece m@(EnPassant piece@(Piece _ color) from@(_, fromRow) to@(toCol, _)) (Board sqs _ wk wq bk bq h _ _)
    = placePiece to piece
    $ deletePiece from
    $ deletePiece (toCol, fromRow) (Board sqs Nothing wk wq bk bq (h ++ [m]) 0 (other color))

movePiece m@(Move (Piece King White) from to) (Board sqs _ _ _ bk bq h cnt _)
    = placePiece to whiteKing $ deletePiece from (Board sqs Nothing False False bk bq (h ++ [m]) (cnt + 1) Black)

movePiece m@(Move (Piece King Black) from to) (Board sqs _ wk wq _ _ h cnt _)
    = placePiece to blackKing $ deletePiece from (Board sqs Nothing wk wq False False (h ++ [m]) (cnt + 1) White)

movePiece m@(Move (Piece Rook White) ('a', 1) to) (Board sqs _ wk _ bk bq h cnt _)
    = placePiece to whiteRook $ deletePiece ('a', 1) (Board sqs Nothing wk False bk bq (h ++ [m]) (cnt + 1) Black)

movePiece m@(Move (Piece Rook White) ('h', 1) to) (Board sqs _ _ wq bk bq h cnt _)
    = placePiece to whiteRook $ deletePiece ('h', 1) (Board sqs Nothing False wq bk bq (h ++ [m]) (cnt + 1) Black)

movePiece m@(Move (Piece Rook Black) ('a', 8) to) (Board sqs _ wk wq bk _ h cnt _)
    = placePiece to blackRook $ deletePiece ('a', 8) (Board sqs Nothing wk wq bk False (h ++ [m]) (cnt + 1) White)

movePiece m@(Move (Piece Rook Black) ('h', 8) to) (Board sqs _ wk wq _ bq h cnt _)
    = placePiece to blackRook $ deletePiece ('h', 8) (Board sqs Nothing wk wq False bq (h ++ [m]) (cnt + 1) White)

movePiece m@(Move (Piece Pawn color) from to) (Board sqs _ wk wq bk bq h cnt _)
    = placePiece to (Piece Pawn color) $ deletePiece from (Board sqs Nothing wk wq bk bq (h ++ [m]) 0 (other color))

movePiece m@(Move piece@(Piece _ color) from to) (Board sqs _ wk wq bk bq h cnt _)
     = placePiece to piece $ deletePiece from (Board sqs Nothing wk wq bk bq (h ++ [m]) (cnt + 1) (other color))

movePiece m@(Capture piece from to) (Board sqs _ wk wq bk bq h _ c)
    = placePiece to piece $ deletePiece from (Board sqs Nothing wk wq bk bq (h ++ [m]) 0 (other c))

movePiece m@(Promotion from to piece) board@(Board sqs _ wk wq bk bq h _ c)
    = placePiece to piece $ deletePiece from (Board sqs Nothing wk wq bk bq (h ++ [m]) 0 (other c))

movePiece m@(CapturePromotion from to piece) (Board sqs _ wk wq bk bq h _ c)
     = placePiece to piece $ deletePiece from (Board sqs Nothing wk wq bk bq (h ++ [m]) 0 (other c))

whitePawnMoveSquares :: Square -> [Square]
whitePawnMoveSquares square@(col, row) = if row == 2 then [(col, 3), (col, 4)] else [(col, row + 1)]

blackPawnMoveSquares :: Square -> [Square]
blackPawnMoveSquares square@(col, row) = if row == 7 then [(col, 6), (col, 5)] else [(col, row - 1)]

pawnMoveFreeSquares :: Square -> Board -> Color -> [Square]
pawnMoveFreeSquares square@(col, row) board color
    | length squares == 2 = if isTaken (head squares) board
                    then []
                    else
                        if isTaken (last squares) board
                            then [head squares]
                            else squares
    | otherwise = if isTaken (head squares) board then [] else squares
    where
        squares = if color == White then whitePawnMoveSquares square else blackPawnMoveSquares square

whitePawnCaptureSquares :: Square -> [Square]
whitePawnCaptureSquares ('a', row) = [('b', succ row)]
whitePawnCaptureSquares ('h', row) = [('g', succ row)]
whitePawnCaptureSquares (col, row) = [(pred col, succ row), (succ col, succ row)]

whitePawnCaptures :: Square -> Board -> [Move]
whitePawnCaptures from@(col, row) board
    | row == 7  = [cp p | cp <- map (CapturePromotion from) squaresTakenByBlack, p <- promotionPieces White]
    | otherwise = map (Capture whitePawn from) squaresTakenByBlack
    where
        squaresTakenByBlack = filter (\s -> isTakenBy s Black board) $ whitePawnCaptureSquares from

blackPawnCaptureSquares :: Square -> [Square]
blackPawnCaptureSquares ('a', row) = [('b', pred row)]
blackPawnCaptureSquares ('h', row) = [('g', pred row)]
blackPawnCaptureSquares (col, row) = [(pred col, pred row), (succ col, pred row)]

blackPawnCaptures :: Square -> Board -> [Move]
blackPawnCaptures from@(col, row) board
    | row == 2  = [cp p | cp <- map (CapturePromotion from) squaresTakenByWhite, p <- promotionPieces Black]
    | otherwise = map (Capture blackPawn from) squaresTakenByWhite
    where
        squaresTakenByWhite = filter (\s -> isTakenBy s White board) [(pred col, pred row), (succ col, pred row)]

promotionPieces :: Color -> [Piece]
promotionPieces color = map (`Piece` color) [Queen, Rook, Bishop, Knight]

promotionMoves :: Color  -> Square -> Square -> [Move]
promotionMoves color from to = map (Promotion from to) $ promotionPieces color

whitePawnMoves :: Square -> Board -> [Move]
whitePawnMoves square@(col, row) board@(Board _ maybeEnPassant _ _ _ _ _ _ _)
    =  whitePawnCaptures square board
    ++ moves
    ++ [EnPassant whitePawn square enPassant | isJust maybeEnPassant && enPassant `elem` whitePawnCaptureSquares square]
    where
        enPassant = fromJust maybeEnPassant
        pawnMoves = pawnMoveFreeSquares square board White
        moves = concatMap (\s@(c, r)-> if row /= 7
                                    then
                                        if (r - row) == 2
                                            then [DoubleSquare square s]
                                            else [Move whitePawn square s]
                                    else promotionMoves White square s) pawnMoves


blackPawnMoves :: Square -> Board -> [Move]
blackPawnMoves square@(col, row) board@(Board _ maybeEnPassant _ _ _ _ _ _ _)
    = blackPawnCaptures square board
    ++ moves
    ++ [EnPassant blackPawn square enPassant | isJust maybeEnPassant && enPassant `elem` blackPawnCaptureSquares square]
    where
        enPassant = fromJust maybeEnPassant
        pawnMoves = pawnMoveFreeSquares square board Black
        moves = concatMap (\s@(c, r) -> if row /= 2
                                    then if (row - r) == 2
                                            then [DoubleSquare square s]
                                            else  [Move blackPawn square s]
                                    else promotionMoves Black square s) pawnMoves

knightMovesSquares :: Square -> [Square]
knightMovesSquares (c, r) = filter isOnBoard
    [ (succ c, succ $ succ r)
    , (succ $ succ c, succ r)
    , (succ $ succ c, pred r)
    , (succ c, pred $ pred r)
    , (pred c, pred $ pred r)
    , (pred $ pred c, pred r)
    , (pred $ pred c, succ r)
    , (pred c, succ $ succ r)
    ]

whiteKnightMoves :: Square -> Board -> [Move]
whiteKnightMoves square board = map (\s -> if isTakenBy s Black board
                                            then Capture whiteKnight square s
                                            else Move whiteKnight square s)
                                            (filter (\s -> not $ isTakenBy s White board) (knightMovesSquares square))

blackKnightMoves :: Square -> Board -> [Move]
blackKnightMoves square board = map (\s -> if isTakenBy s White board
                                            then Capture blackKnight square s
                                            else Move blackKnight square s)
                                            (filter (\s -> not $ isTakenBy s Black board) (knightMovesSquares square))

kingMovesSquares :: Square -> [Square]
kingMovesSquares (c, r) = filter isOnBoard [(c, succ r), (succ c, succ r),
                                            (succ c, r), (succ c, pred r),
                                            (c, pred r), (pred c, pred r),
                                            (pred c, r), (pred c, succ r)]

whiteKingSideCastling :: Board -> Bool
whiteKingSideCastling b@(Board sqs _ wk _ _ _ _ _ _)
        = wk && not (any (\s -> isTaken s b || s `elem` capturesSquares) [('e', 1), ('f', 1), ('g', 1)])
    where
        captures = piecesCaptures b Black
        capturesSquares = map toSquare captures

blackKingSideCastling :: Board -> Bool
blackKingSideCastling b@(Board sqs _ _ _ bk _ _ _ _)
        = bk && not (any (\s -> isTaken s b || s `elem` capturesSquares) [('e', 8), ('f', 8), ('g', 8)])
    where
        captures = piecesCaptures b White
        capturesSquares = map toSquare captures

whiteQueenSideCastling :: Board -> Bool
whiteQueenSideCastling b@(Board sqs _ _ wq _ _ _ _ _)
        = wq && not (any (\s -> isTaken s b || s `elem` capturesSquares) [('e', 1), ('d', 1), ('c', 1)])
    where
        captures = piecesCaptures b Black
        capturesSquares = map toSquare captures

blackQueenSideCastling :: Board -> Bool
blackQueenSideCastling b@(Board sqs _ _ _ _ bq _ _ _)
        = bq && not (any (\s -> isTaken s b || s `elem` capturesSquares) [('e', 8), ('d', 8), ('c', 8)])
    where
        captures = piecesCaptures b White
        capturesSquares = map toSquare captures

whiteKingMoves :: Square -> Board -> [Move]
whiteKingMoves square board = ks ++ qs ++ map (\s -> if isTakenBy s Black board
                                           then Capture whiteKing square s
                                           else Move whiteKing square s) (filter (\s -> not (isTakenBy s White board)
                                                                            && (s `notElem` capturesSquares)) $ kingMovesSquares square)
    where
        captures = piecesCaptures board Black
        capturesSquares = map toSquare captures
        ks = [KingsideCastling White | whiteKingSideCastling board]
        qs = [QueensideCastling White | whiteQueenSideCastling board]

blackKingMoves :: Square -> Board -> [Move]
blackKingMoves square board = ks ++ qs ++ map (\s -> if isTakenBy s White board
                                           then Capture blackKing square s
                                           else Move blackKing square s) (filter (\s -> not (isTakenBy s Black board)
                                                                            && (s `notElem` capturesSquares)) $ kingMovesSquares square)
    where
        captures = piecesCaptures board White
        capturesSquares = map toSquare captures
        ks = [KingsideCastling Black | blackKingSideCastling board]
        qs = [QueensideCastling Black | blackQueenSideCastling board]

rookMovesSquares :: Square -> [[Square]]
rookMovesSquares square@(c, r) = [top, down, right, left]
    where
        top = filter (/= square) (zip [c, c, c, c, c, c, c, c] [r .. 8])
        down = filter (/= square) (zip [c, c, c, c, c, c, c, c] [1 .. r])
        right = filter (/= square) (zip [c .. 'h'] [r, r, r, r, r, r, r, r])
        left = filter (/= square) (zip ['a' .. c] [r, r, r, r, r, r, r, r])

whiteRookMoves :: Square -> Board -> [Move]
whiteRookMoves square board = map (\s -> if isTakenBy s Black board
                                            then Capture whiteRook square s
                                            else Move whiteRook square s)  (top ++ down ++ right ++ left)
    where
        sqs = rookMovesSquares square
        top = filterAllEmptyOrFirstOpposite board White (head sqs)
        down = filterAllEmptyOrFirstOpposite board White (reverse (sqs !! 1))
        right = filterAllEmptyOrFirstOpposite board White (sqs !! 2)
        left = filterAllEmptyOrFirstOpposite board White (reverse (sqs !! 3))

blackRookMoves :: Square -> Board -> [Move]
blackRookMoves square board = map (\s -> if isTakenBy s White board
                                            then Capture blackRook square s
                                            else Move blackRook square s) (top ++ down ++ right ++ left)
    where
        sqs = rookMovesSquares square
        top = filterAllEmptyOrFirstOpposite board Black (head sqs)
        down = filterAllEmptyOrFirstOpposite board Black (reverse (sqs !! 1))
        right = filterAllEmptyOrFirstOpposite board Black (sqs !! 2)
        left = filterAllEmptyOrFirstOpposite board Black (reverse (sqs !! 3))

bishopTopSquares :: Square -> [Square] -> [Square]
bishopTopSquares square@(c, r) sqs = if not $ isOnBoard square then filter (/= square) sqs
                                                                else bishopTopSquares (succ c, succ r) (sqs ++ [square])

bishopDownSquares :: Square -> [Square] -> [Square]
bishopDownSquares square@(c, r) sqs = if not $ isOnBoard square then filter (/= square) sqs
                                                                else bishopDownSquares (pred c, pred r) (sqs ++ [square])

bishopRightSquares :: Square -> [Square] -> [Square]
bishopRightSquares square@(c, r) sqs = if not $ isOnBoard square then filter (/= square) sqs
                                                                else bishopRightSquares (succ c, pred r) (sqs ++ [square])

bishopLeftSquares :: Square -> [Square] -> [Square]
bishopLeftSquares square@(c, r) sqs = if not $ isOnBoard square then filter (/= square) sqs
                                                                else bishopLeftSquares (pred c, succ r) (sqs ++ [square])

whiteBishopMoves :: Square -> Board -> [Move]
whiteBishopMoves square board = map (\s -> if isTakenBy s Black board
                                            then Capture whiteBishop square s
                                            else Move whiteBishop square s)  (top ++ down ++ right ++ left)
    where
        top = filterAllEmptyOrFirstOpposite board White (filter (/= square) (bishopTopSquares square []))
        down = filterAllEmptyOrFirstOpposite board White (filter (/= square) (bishopDownSquares square []))
        right = filterAllEmptyOrFirstOpposite board White (filter (/= square) (bishopRightSquares square []))
        left = filterAllEmptyOrFirstOpposite board White (filter (/= square) (bishopLeftSquares square []))

blackBishopMoves :: Square -> Board -> [Move]
blackBishopMoves square board = map (\s -> if isTakenBy s White board
                                            then Capture blackBishop square s
                                            else Move blackBishop square s)  (top ++ down ++ right ++ left)
    where
        top = filterAllEmptyOrFirstOpposite board Black (filter (/= square) (bishopTopSquares square []))
        down = filterAllEmptyOrFirstOpposite board Black (filter (/= square) (bishopDownSquares square []))
        right = filterAllEmptyOrFirstOpposite board Black (filter (/= square) (bishopRightSquares square []))
        left = filterAllEmptyOrFirstOpposite board Black (filter (/= square) (bishopLeftSquares square []))

whiteQueenMoves :: Square -> Board -> [Move]
whiteQueenMoves square board = map (\m -> if not (isCapture m) then Move whiteQueen square $ toSquare m
                                                                else Capture whiteQueen square $ toSquare m)
                                    (whiteBishopMoves square board ++ whiteRookMoves square board)

isCapture :: Move -> Bool
isCapture Capture {} = True
isCapture _ = False

blackQueenMoves :: Square -> Board -> [Move]
blackQueenMoves square board = map (\m -> if not (isCapture m) then Move blackQueen square $ toSquare m
                                                                else Capture blackQueen square $ toSquare m)
                                    (blackBishopMoves square board ++ blackRookMoves square board)

filterAllEmptyOrFirstOpposite :: Board -> Color -> [Square] -> [Square]
filterAllEmptyOrFirstOpposite board color [] = []
filterAllEmptyOrFirstOpposite board color (square:squares) = case findPiece board square of
  Nothing          -> square : filterAllEmptyOrFirstOpposite board color squares
  Just (Piece _ c) -> [square | c /= color]

isTaken :: Square -> Board -> Bool
isTaken square board = isTakenBy square White board || isTakenBy square Black board

isTakenBy :: Square -> Color -> Board -> Bool
isTakenBy square color (Board squares _ _ _ _ _ _ _ _) = case M.lookup square squares of
    Nothing -> False
    Just (Piece _ c) -> color == c

pieceSquares :: Board -> Color -> [Square]
pieceSquares board@(Board squares _ _ _ _ _ _ _ _) color = map fst $ filter (\(_, Piece _ c) -> c == color) $ M.toList squares

pieces :: Board -> Color -> [Piece]
pieces board@(Board squares _ _ _ _ _ _ _ _) color = map snd $ filter (\(_, Piece _ c) -> c == color) $ M.toList squares

possibleMoves :: Board -> Square -> [Move]
possibleMoves board@(Board squares _ _ _ _ _ _ _ _) square = case M.lookup square squares of
    Nothing             -> []
    Just (Piece Pawn White) -> whitePawnMoves square board
    Just (Piece Pawn Black) -> blackPawnMoves square board
    Just (Piece Knight White) -> whiteKnightMoves square board
    Just (Piece Knight Black) -> blackKnightMoves square board
    Just (Piece King White) -> whiteKingMoves square board
    Just (Piece King Black) -> blackKingMoves square board
    Just (Piece Bishop White) -> whiteBishopMoves square board
    Just (Piece Bishop Black) -> blackBishopMoves square board
    Just (Piece Rook White) -> whiteRookMoves square board
    Just (Piece Rook Black) -> blackRookMoves square board
    Just (Piece Queen White) -> whiteQueenMoves square board
    Just (Piece Queen Black) -> blackQueenMoves square board

pieceCaptures :: Board -> Square -> [Move]
pieceCaptures board@(Board squares _ _ _ _ _ _ _ _) square = case M.lookup square squares of
    Nothing             -> []
    Just (Piece Pawn White) -> map (Capture whitePawn square) $ whitePawnCaptureSquares square
    Just (Piece Pawn Black) -> map (Capture blackPawn square) $ blackPawnCaptureSquares square
    Just (Piece Knight White) -> whiteKnightMoves square board
    Just (Piece Knight Black) -> blackKnightMoves square board
    Just (Piece King White) -> map (Capture whiteKing square) (kingMovesSquares square)
    Just (Piece King Black) -> map (Capture blackKing square) (kingMovesSquares square)
    Just (Piece Bishop White) -> whiteBishopMoves square board
    Just (Piece Bishop Black) -> blackBishopMoves square board
    Just (Piece Rook White) -> whiteRookMoves square board
    Just (Piece Rook Black) -> blackRookMoves square board
    Just (Piece Queen White) -> whiteQueenMoves square board
    Just (Piece Queen Black) -> blackQueenMoves square board

piecesCaptures :: Board -> Color -> [Move]
piecesCaptures board color = concatMap (pieceCaptures board) $ pieceSquares board color

allPossibleMoves :: Board -> Color -> [Move]
allPossibleMoves board color = filter (\m -> not (isCheck (movePiece m board) color))
                                        (concatMap (possibleMoves board) $ pieceSquares board color)

kingSquare :: Board -> Color -> Square
kingSquare (Board squares _ _ _ _ _ _ _ _) color = fst $ head $ filter (\(s, p) -> p == Piece King color) $ M.toList squares

isCheck :: Board -> Color -> Bool
isCheck board color = square `elem` capturesSquares
    where
        square = kingSquare board color
        captures = piecesCaptures board (other color)
        capturesSquares = map toSquare captures

isMate :: Board -> Color -> Bool
isMate board color = null (allPossibleMoves board color) && isCheck board color

isDrawWhite :: Board -> Color -> Bool
isDrawWhite _ Black = False
isDrawWhite board _ = null (allPossibleMoves board White)

isDrawBlack :: Board -> Color -> Bool
isDrawBlack _ White = False
isDrawBlack board _ = null (allPossibleMoves board Black)

isDraw :: Board -> Color -> Bool
isDraw board color = isDrawWhite board color || isDrawBlack board color || (length psw == 1 && length psb == 1)
        || (delete whiteKing psw == [whiteKnight] && null (delete blackKing psb))
        || (null (delete whiteKing psw) && delete blackKing psb == [blackKnight])
        || (delete whiteKing psw == [whiteBishop] && null (delete blackKing psb))
        || (null (delete whiteKing psw) && delete blackKing psb == [blackBishop])
    where
        psw = pieces board White
        psb = pieces board Black

pgnSquare :: Square -> String
pgnSquare (c, r) = c : show r

pgnPiece :: Piece -> String
pgnPiece (Piece Pawn _) = ""
pgnPiece (Piece Knight _) = "N"
pgnPiece (Piece Bishop _) = "B"
pgnPiece (Piece Rook _) = "R"
pgnPiece (Piece Queen _) = "Q"
pgnPiece (Piece King _) = "K"

pgnMove :: Move -> String
pgnMove (DoubleSquare from to) = pgnSquare from ++ pgnSquare to
pgnMove (Move p to from) = pgnPiece p ++ pgnSquare to ++ pgnSquare from
pgnMove (KingsideCastling _) = "O-O"
pgnMove (QueensideCastling _) = "O-O-O"
pgnMove (Capture p to from) = pgnPiece p ++ pgnSquare to ++ "x" ++ pgnSquare from
pgnMove (EnPassant _ from to) = pgnSquare from ++ "x" ++ pgnSquare to
pgnMove (Promotion from to p) = pgnSquare from ++ pgnSquare to ++ "=" ++ pgnPiece p
pgnMove (CapturePromotion to from p) = pgnSquare to ++ "x" ++ pgnSquare from ++ "=" ++ pgnPiece p

toPGN :: [Move] -> String
toPGN moves = unwords $ export 1 moves
  where
    export :: Int -> [Move] -> [String]
    export moveNo []       = []
    export moveNo [w]      = (show moveNo ++ ".") : [pgnMove w]
    export moveNo (w:b:ms) = (show moveNo ++ ".") : pgnMove w : pgnMove b : export (succ moveNo) ms



evalPiece :: Piece -> Double
evalPiece (Piece Pawn _) = 1
evalPiece (Piece King _) = 0
evalPiece (Piece Knight _) = 3
evalPiece (Piece Bishop _) = 3
evalPiece (Piece Rook _) = 5
evalPiece (Piece Queen _) = 9

evalFn :: Board -> Color -> Double
evalFn board White
  | isMate board White = -999
  | isMate board Black = 999
  | isDraw board White = 0
  | otherwise = (sum (map evalPiece piecesWhite) - sum (map evalPiece piecesBlack))
  where
      piecesWhite = pieces board White
      piecesBlack = pieces board Black
evalFn board Black
  | isMate board White = 999
  | isMate board Black = -999
  | isDraw board Black = 0
  | otherwise = -(sum (map evalPiece piecesWhite) - sum (map evalPiece piecesBlack))

  where
      piecesWhite = pieces board White
      piecesBlack = pieces board Black

minimax :: Board -> Int -> Color -> Move
minimax board depth player = snd (maxValue board depth player)

maxValue :: Board -> Int -> Color -> (Double, Move)
maxValue board@(Board _ _ _ _ _ _ h _ _) depth player
    = if isMate board player || isDraw board player || (depth == 0) then (evalFn board player, last h)
        else last $ sortOn fst (map (maxHelper board depth player) (allPossibleMoves board player))

maxHelper :: Board -> Int ->  Color -> Move -> (Double, Move)
maxHelper board@(Board _ _ _ _ _ _ h _ _) depth player move = (-v2, move)
    where
        (v2, a2) = minValue (movePiece move board) (depth - 1) (other player)

minValue :: Board -> Int -> Color -> (Double, Move)
minValue board@(Board _ _ _ _ _ _ h _ _) depth player
    = if isMate board player || isDraw board player || (depth == 0) then (evalFn board player, last h)
        else last $ sortOn fst (map (minHelper board depth player) (allPossibleMoves board player))

minHelper :: Board -> Int -> Color -> Move -> (Double, Move)
minHelper board@(Board _ _ _ _ _ _ h _ _) depth player move = (-v2, move)
    where
        (v2, a2) = maxValue (movePiece move board) (depth - 1) (other player)

lastOfQuad :: (Double, Double, Double, Move) -> Move
lastOfQuad (_, _, _, m) = m

absEvalFn :: Board -> Double
absEvalFn board
        | isMate board White = -999.0
        | isMate board Black = 999.0
        | otherwise = sum (map evalPiece piecesWhite) - sum (map evalPiece piecesBlack)
  where
      piecesWhite = pieces board White
      piecesBlack = pieces board Black
{-
alphaBetaSearch :: Board -> Int -> Color -> Maybe Move
alphaBetaSearch board depth White = lastOfQuad $ alphaValue board depth White (-999) 999
alphaBetaSearch board depth Black = lastOfQuad $ betaValue board depth Black 999 (-999)

alphaValue :: Board -> Int -> Color -> Int -> Int -> (Int, Int, Int, Maybe Move)
alphaValue board@(Board _ _ _ _ _ _ h _ _) depth player alpha beta
    | isMate board player || isDraw board player || depth == 0 = (alpha, beta, absEvalFn board, Just $ last h)
    | otherwise = (alpha', beta', v', best')
  where

    (alpha', beta', v', best', _) = foldl (\result@(a, b, v, best, break) current -> if break then result else alphaHelper board depth player current best v a b) (alpha, beta, -999, Nothing, False) allMoves
    allMoves = allPossibleMoves board player

alphaHelper :: Board -> Int -> Color -> Move -> Maybe Move -> Int -> Int -> Int -> (Int, Int, Int, Maybe Move, Bool)
alphaHelper board@(Board _ _ _ _ _ _ h _ _) depth player currentMove bestMove v alpha beta = traceShow (alpha'', beta', v'', bestMove', break) (alpha'', beta', v'', bestMove', break)
  where
    (alpha', beta', v', m') = betaValue (movePiece currentMove board) (depth - 1) (other player) alpha beta
    (alpha'', v'', bestMove') =
        if v' > v then (max alpha' v', v', m')
            else (alpha', v, bestMove)

    break = v'' > beta'
    --break = False

betaValue :: Board -> Int -> Color -> Int -> Int -> (Int, Int, Int, Maybe Move)
betaValue board@(Board _ _ _ _ _ _ h _ _) depth player alpha beta
    | isMate board player || isDraw board player || depth == 0 = (alpha, beta, absEvalFn board, Just $ last h)
    | otherwise = (alpha', beta', v', best')
  where
    (alpha', beta', v', best', _) = foldl (\result@(a, b, v, best, break) current -> if break then result else betaHelper board depth player current best v a b) (alpha, beta, 999, Nothing, False) allMoves
    allMoves = allPossibleMoves board player

betaHelper :: Board -> Int -> Color -> Move -> Maybe Move -> Int -> Int -> Int -> (Int, Int, Int, Maybe Move, Bool)
betaHelper board@(Board _ _ _ _ _ _ h _ _) depth player currentMove bestMove v alpha beta = (alpha', beta'', v'', bestMove', break)
  where
    (alpha', beta', v', m') = alphaValue (movePiece currentMove board) (depth - 1) (other player) alpha beta
    (beta'', v'', bestMove') = 
        if v' < v then (min beta' v', v', m')
            else (beta', v, bestMove)

    break = alpha' < v''
    --break = False
 -}

debugOffset :: Int -> Int -> String
debugOffset maxDepth depth = foldl (\r _ -> r ++ "  ") (show depth) [1..maxDepth - depth]

alphaBetaSearch :: Board -> Int -> Color -> Move
alphaBetaSearch board depth White = lastOfQuad $ alphaValue board depth White 0.0000001 0 (-10000.0) 10000.0
alphaBetaSearch board depth Black = lastOfQuad $ betaValue board depth Black  0 0.0000001 (-10000.0) 10000.0

alphaValue :: Board -> Int -> Color -> Double -> Double -> Double -> Double -> (Double, Double, Double, Move)
alphaValue board@(Board _ _ _ _ _ _ h _ _) depth player wha bla alpha beta
    = if isMate board player || isDraw board player || depth == 0 then (alpha, beta, absEvalFn board, last h)
        else (alpha' - 0.001, beta', v' - wha, best')
    where
        (alpha', beta', v', best', _) = foldl (\result@(a, b, v, best, break) current -> if break then result else traceShow (debugOffset 5 depth, current) alphaHelper board depth player current best v a b wha bla) (alpha, beta, -999, head allMoves, False) allMoves
        allMoves = allPossibleMoves board player
        --allMoves = traceShow (debugOffset 5 depth, allPossibleMoves board player) allPossibleMoves board player

alphaHelper :: Board -> Int -> Color -> Move -> Move -> Double -> Double -> Double -> Double -> Double -> (Double, Double, Double, Move, Bool)
alphaHelper board@(Board _ _ _ _ _ _ h _ _) depth player currentMove bestMove v alpha beta wha bla = traceShow (debugOffset 5 depth, alpha'', beta', v'', bestMove', break) (alpha'', beta', v'', bestMove', break)
    where
        (alpha', beta', v', m') = betaValue (movePiece currentMove board) (depth - 1) (other player) wha bla alpha beta

        (alpha'', v'', bestMove')
            = if max v' v > beta'
                then
                    if v' > v
                        then (alpha', v', currentMove)
                        else (alpha', v, bestMove)
                else
                    if v' > v
                        then (max alpha' v', v', currentMove)
                        else (alpha', v, bestMove)

        break = v'' >= beta'
        --break = traceShow (debugOffset 5 depth, v'', ">=", beta') v'' >= beta'

betaValue :: Board -> Int -> Color -> Double -> Double -> Double -> Double -> (Double, Double, Double, Move)
betaValue board@(Board _ _ _ _ _ _ h _ _) depth player wha bla alpha beta
    = if isMate board player || isDraw board player || depth == 0 then (alpha, beta, absEvalFn board, last h)
        else (alpha', beta' + 0.001, v' + bla, best')
    where
        (alpha', beta', v', best', _) = foldl (\result@(a, b, v, best, break) current -> if break then result else traceShow (debugOffset 5 depth, current) betaHelper board depth player current best v a b wha bla) (alpha, beta, 999, head allMoves, False) allMoves
        allMoves = allPossibleMoves board player
        --allMoves = traceShow (debugOffset 5 depth, allPossibleMoves board player) allPossibleMoves board player

betaHelper :: Board -> Int ->  Color -> Move -> Move -> Double -> Double -> Double -> Double -> Double -> (Double, Double, Double, Move, Bool)
betaHelper board@(Board _ _ _ _ _ _ h _ _) depth player currentMove bestMove v alpha beta wha bla = traceShow (debugOffset 5 depth, alpha', beta'', v'', bestMove', break) (alpha', beta'', v'', bestMove', break)
    where
        (alpha', beta', v', m') = alphaValue (movePiece currentMove board) (depth - 1) (other player) wha bla alpha beta
        --(beta'', v'', bestMove') = if min v' v <= alpha' then (min beta' v', v', currentMove) else (beta', v, bestMove)

        (beta'', v'', bestMove')
            = if min v' v < alpha'
                then
                    if v' < v
                        then (beta', v', currentMove)
                        else (beta', v, bestMove)
                else
                    if v' < v
                        then (min beta' v', v', currentMove)
                        else (beta', v, bestMove)

        break = v'' <= alpha'

        --break = traceShow (debugOffset 5 depth, v'', "<=", alpha') v'' <= alpha'

{-
runAlphaHelper :: (Move, Move, Bool) -> [Move] -> Board -> Int -> Color -> Int -> Int -> (Move, Move, Int, Bool)
runAlphaHelper (v, bestMove, break) [] _ _ _ a _ = (v, bestMove, a, break)
runAlphaHelper (v, bestMove, break) (currentMove:ms) board d p a b | break = (v, bestMove, a, break)
                                                                   | otherwise = alphaHelper board d p currentMove bestMove v a b
-}



enPassantToFEN :: Maybe Square -> String
enPassantToFEN = maybe "-" pgnSquare

colorToFEN :: Color -> String
colorToFEN White = "w"
colorToFEN Black = "b"

castlingToFEN :: Board -> String
castlingToFEN board@(Board _ _ wk wq bk bq _ _ _) = (if wk then "K" else "") ++ (if wq then "Q" else "") ++ (if bk then "k" else "") ++ (if bq then "q" else "")

toFEN :: Board -> String
toFEN board@(Board _ enPassant wk wq bk bq h cnt c) = intercalate "/" (map (rowToFEN board) (reverse rows))
        ++ " " ++ colorToFEN c ++ " " ++ castlingToFEN board ++ " " ++ enPassantToFEN enPassant ++ " " ++ show cnt ++ " " ++ show (length h `div` 2)

rowToFEN :: Board -> Int -> String
rowToFEN board i = if emptySquares == 0 then fen else fen ++ show emptySquares
    where
        sqs = map (, i) cols
        (emptySquares, fen) = foldl colsToFEN  (0, "") (map (findPiece board) sqs)

colsToFEN (emptyCount, fen) i = case i of
     Nothing -> (emptyCount + 1, fen)
     Just p -> (0, fen ++ (if emptyCount == 0 then "" else show emptyCount) ++  exportPiece p)

digitOrNot :: String -> Bool
digitOrNot "1" = True
digitOrNot "2" = True
digitOrNot "3" = True
digitOrNot "4" = True
digitOrNot "5" = True
digitOrNot "6" = True
digitOrNot "7" = True
digitOrNot "8" = True
digitOrNot "9" = True
digitOrNot _ = False

---colToFEN :: String -> Int -> String
---ColToFEN string i = if digitOrNot (string !! i) then if digitOrNot (string !! (i + 1) )

exportPiece :: Piece -> String
exportPiece (Piece Pawn White) = "P"
exportPiece (Piece Pawn Black) = "p"
exportPiece (Piece Knight White) = "N"
exportPiece (Piece Knight Black) = "n"
exportPiece (Piece Bishop White) = "B"
exportPiece (Piece Bishop Black) = "b"
exportPiece (Piece Rook White) = "R"
exportPiece (Piece Rook Black) = "r"
exportPiece (Piece Queen White) = "Q"
exportPiece (Piece Queen Black) = "q"
exportPiece (Piece King White) = "K"
exportPiece (Piece King Black) = "k"

importPiece :: Char -> Color -> Piece
importPiece 'N' color = Piece Knight color
importPiece 'B' color = Piece Bishop color
importPiece 'R' color = Piece Rook color
importPiece 'Q' color = Piece Queen color
importPiece 'K' color = Piece King color

parseMove :: String -> Color -> Maybe Move
parseMove [fromCol, '2', toCol, '4'] White

    = Just $ DoubleSquare (fromCol, 2) (toCol, 4)
parseMove [fromCol, '7', toCol, '5'] Black
    = Just $ DoubleSquare (fromCol, 7) (toCol, 5)
parseMove [fromCol, fromRow, toCol, toRow] color = Just $ Move (Piece Pawn color) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMove [fromCol, fromRow, 'x', toCol, toRow, '=', piece] color
    = Just $ CapturePromotion (fromCol, digitToInt fromRow) (toCol, digitToInt toRow) (importPiece piece color)
parseMove [fromCol, fromRow, 'x', toCol, toRow] color = Just $ Capture (Piece Pawn color) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMove [piece, fromCol, fromRow, 'x', toCol, toRow] color = Just $ Capture (importPiece piece color) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMove [piece, fromCol, fromRow, toCol, toRow] color = Just $ Move (importPiece piece color) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMove [piece, fromCol, fromRow, 'x', toCol, toRow] color = Just $ Capture (importPiece piece color) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMove [fromCol, fromRow, toCol, toRow, '=', piece] color = Just $ Promotion (fromCol, digitToInt fromRow) (toCol, digitToInt toRow) (importPiece piece color)
parseMove "O-O-O" color = Just $ QueensideCastling color
parseMove "O-O" color = Just $ KingsideCastling color
parseMove _ _ = Nothing

parseMovePGN :: String -> Color -> Move
parseMovePGN [fromCol, '2', toCol, '4'] White

    = DoubleSquare (fromCol, 2) (toCol, 4)
parseMovePGN [fromCol, '7', toCol, '5'] Black
    = DoubleSquare (fromCol, 7) (toCol, 5)
parseMovePGN [fromCol, fromRow, toCol, toRow] color = Move (Piece Pawn color) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMovePGN [fromCol, fromRow, 'x', toCol, toRow, '=', piece] color
    = CapturePromotion (fromCol, digitToInt fromRow) (toCol, digitToInt toRow) (importPiece piece color)
parseMovePGN [fromCol, fromRow, 'x', toCol, toRow] color = Capture (Piece Pawn color) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMovePGN [piece, fromCol, fromRow, 'x', toCol, toRow] color = Capture (importPiece piece color) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMovePGN [piece, fromCol, fromRow, toCol, toRow] color = Move (importPiece piece color) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMovePGN [piece, fromCol, fromRow, 'x', toCol, toRow] color = Capture (importPiece piece color) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMovePGN [fromCol, fromRow, toCol, toRow, '=', piece] color = Promotion (fromCol, digitToInt fromRow) (toCol, digitToInt toRow) (importPiece piece color)
parseMovePGN "O-O-O" color = QueensideCastling color
parseMovePGN "O-O" color = KingsideCastling color

splitMoves :: String -> [Text]
splitMoves s = foldr (\(i, m) r -> if i `mod` 3 /= 0 then m : r else r) [] $ zip [0..] $ splitOn " " t
    where t = pack s

parseMoves :: [Text] -> Board -> Color -> Board
parseMoves [] board _ = board
parseMoves (m:moves) board color = parseMoves moves (movePiece (parseMovePGN (unpack m) color) board) (other color)