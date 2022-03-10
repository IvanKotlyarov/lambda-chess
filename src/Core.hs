module Core where

import           Data.Maybe (fromJust, isJust)
import qualified Data.Map as M

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

data Board = Board (M.Map Square Piece) (Maybe Square)
  deriving (Eq, Show)

squares :: Board -> M.Map Square Piece
squares board@(Board sqs _) = sqs

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
emptyBoard = Board M.empty Nothing

initialBoard :: Board
initialBoard = Board (M.fromList (whitePawns ++ pieces White ++ blackPawns ++ pieces Black)) Nothing
    where
        whitePawns = map (\c -> ((c, 2), whitePawn)) cols
        blackPawns = map (\c -> ((c, 7), blackPawn)) cols
        row color = if color == White then 1 else 8
        pieces :: Color -> [(Square, Piece)]
        pieces color = zipWith (\c p -> ((c, row color), p)) cols
                [ Piece Rook color, Piece Knight color, Piece Bishop color, Piece Queen color
                , Piece King color, Piece Bishop color, Piece Knight color, Piece Rook color
                ]

isOnBoard :: Square -> Bool
isOnBoard square@(col, row) = if (col `elem` ['a' .. 'h']) && (row `elem` [1 .. 8]) then True else False

placePiece :: Square -> Piece -> Board -> Board
placePiece square piece (Board squares enPassant) = Board (M.insert square piece squares) enPassant

deletePiece :: Square -> Board -> Board
deletePiece square (Board squares enPassant) = Board (M.delete square squares) enPassant

movePiece :: Move -> Board -> Board
movePiece (KingsideCastling color) (Board sqs _)
    = placePiece ('f', row) (Piece Rook color)
    $ deletePiece ('h', row)
    $ placePiece ('g', row) (Piece King color)
    $ deletePiece ('e', row) (Board sqs Nothing)
    where
        row = if color == White then 1 else 8

movePiece (QueensideCastling color) (Board sqs _)
    = placePiece ('d', row) (Piece Rook color)
    $ deletePiece ('a', row)
    $ placePiece ('c', row) (Piece King color)
    $ deletePiece ('e', row) (Board sqs Nothing)
    where
        row = if color == White then 1 else 8

movePiece (DoubleSquare from@(fromCol, fromRow) to@(toCol, _)) board
    = Board sqs $ Just enPassant
    where
        sqs = squares $ placePiece to (if fromRow == 2 then whitePawn else blackPawn)
            $ deletePiece from
            $ deletePiece (toCol, fromRow) board
        enPassant = if fromRow == 2 then (fromCol, succ fromRow) else (fromCol, pred fromRow)

movePiece (EnPassant piece@(Piece _ color) from@(_, fromRow) to@(toCol, _)) (Board sqs _)
    = placePiece to piece
    $ deletePiece from
    $ deletePiece (toCol, fromRow) (Board sqs Nothing)

movePiece (Move piece from to) (Board sqs _) = placePiece to piece $ deletePiece from (Board sqs Nothing)

movePiece (Capture piece from to) (Board sqs _) =  placePiece to piece $ deletePiece from (Board sqs Nothing)

movePiece (Promotion from to piece) board = placePiece to piece $ deletePiece from board

movePiece (CapturePromotion from to piece) (Board sqs _) = placePiece to piece $ deletePiece from (Board sqs Nothing)

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
whitePawnMoves square@(col, row) board@(Board _ maybeEnPassant)
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
blackPawnMoves square@(col, row) board@(Board _ maybeEnPassant)
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
knightMovesSquares square@(c, r) = concatMap (\s -> [s | isOnBoard s]) [(succ c, succ $ succ r), (succ $ succ c, succ r),
                                                                  (succ $ succ c, pred r), (succ c, pred $ pred r),
                                                                  (pred c, pred $ pred r), (pred $ pred c, pred r),
                                                                  (pred $ pred c, succ r), (pred c, succ $ succ r)]

isTaken :: Square -> Board -> Bool
isTaken square board = isTakenBy square White board || isTakenBy square Black board

isTakenBy :: Square -> Color -> Board -> Bool
isTakenBy square color (Board squares _) = case M.lookup square squares of
    Nothing -> False
    Just (Piece _ c) -> color == c

pieceSquares :: Board -> Color -> [Square]
pieceSquares board@(Board squares _) color = map fst $ filter (\(_, Piece _ c) -> c == color) $ M.toList squares

possibleMoves :: Board -> Square -> [Move]
possibleMoves board@(Board squares _) square = case M.lookup square squares of
    Nothing             -> []
    Just (Piece Pawn White) -> whitePawnMoves square board
    Just (Piece Pawn Black) -> blackPawnMoves square board
    _                       -> []

allPossibleMoves :: Board -> Color -> [Move]
allPossibleMoves board color = concatMap (possibleMoves board) $ pieceSquares board color