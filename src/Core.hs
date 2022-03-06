module Core where

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

data Board = Board (M.Map Square Piece)
  deriving (Eq, Show)

-- d8=Q   -- простое продвижение
-- cxd8=Q -- взятие с последующим продвижением

data Move
    = Move Piece Square Square
    -- | DoubleSquare Square
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
emptyBoard = Board M.empty

initialBoard :: Board
initialBoard = Board $ M.fromList (whitePawns ++ pieces White ++ blackPawns ++ pieces Black)
    where
        whitePawns = map (\c -> ((c, 2), whitePawn)) cols
        blackPawns = map (\c -> ((c, 7), blackPawn)) cols
        row color = if color == White then 1 else 8
        pieces :: Color -> [(Square, Piece)]
        pieces color = zipWith (\c p -> ((c, row color), p)) cols 
                [ Piece Rook color, Piece Knight color, Piece Bishop color, Piece Queen color
                , Piece King color, Piece Bishop color, Piece Knight color, Piece Rook color
                ]

placePiece :: Square -> Piece -> Board -> Board
placePiece square piece (Board squares) = Board $ M.insert square piece squares

deletePiece :: Square -> Board -> Board
deletePiece square (Board squares) = Board $ M.delete square squares

movePiece :: Move -> Board -> Board
movePiece (KingsideCastling color) board
    = placePiece ('f', row) (Piece Rook color)
    $ deletePiece ('h', row)
    $ placePiece ('g', row) (Piece King color)
    $ deletePiece ('e', row) board
    where
        row = if color == White then 1 else 8

movePiece (QueensideCastling color) board
    = placePiece ('d', row) (Piece Rook color)
    $ deletePiece ('a', row)
    $ placePiece ('c', row) (Piece King color)
    $ deletePiece ('e', row) board
    where
        row = if color == White then 1 else 8

movePiece (EnPassant piece@(Piece _ color) from@(_, fromRow) to@(toCol, _)) board
    = placePiece to piece
    $ deletePiece from
    $ deletePiece (toCol, fromRow) board

movePiece (Move piece from to) board = placePiece to piece $ deletePiece from board

movePiece (Capture piece from to) board =  placePiece to piece $ deletePiece from board

movePiece (Promotion from to piece) board = placePiece to piece $ deletePiece from board

movePiece (CapturePromotion from to piece) board = placePiece to piece $ deletePiece from board

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

whitePawnCaptures :: Square -> Board -> [Move]
whitePawnCaptures from@(col, row) board
    | row == 7  = [cp p | cp <- map (CapturePromotion from) squaresTakenByBlack, p <- promotionPieces White]
    | otherwise = map (Capture whitePawn from) squaresTakenByBlack
    where
        squaresTakenByBlack = filter (\s -> isTakenBy s Black board) [(pred col, succ row), (succ col, succ row)]

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
whitePawnMoves square@(col, row) board = whitePawnCaptures square board ++ moves
    where
        pawnMoves = pawnMoveFreeSquares square board White
        moves = concatMap (\s -> if row /= 7
                                    then [Move whitePawn square s]
                                    else promotionMoves White square s) pawnMoves

blackPawnMoves :: Square -> Board -> [Move]
blackPawnMoves square@(col, row) board = blackPawnCaptures square board ++ moves
    where
        pawnMoves = pawnMoveFreeSquares square board Black
        moves = concatMap (\s -> if row /= 2
                                    then [Move blackPawn square s]
                                    else promotionMoves Black square s) pawnMoves

isTaken :: Square -> Board -> Bool
isTaken square board = isTakenBy square White board || isTakenBy square Black board

isTakenBy :: Square -> Color -> Board -> Bool
isTakenBy square color (Board squares) = case M.lookup square squares of
    Nothing -> False
    Just (Piece _ c) -> color == c

pieceSquares :: Board -> Color -> [Square]
pieceSquares board@(Board squares) color = map fst $ filter (\(_, Piece _ c) -> c == color) $ M.toList squares

possibleMoves :: Board -> Square -> [Move]
possibleMoves board@(Board squares) square = case M.lookup square squares of
    Nothing             -> []
    Just (Piece Pawn White) -> whitePawnMoves square board
    Just (Piece Pawn Black) -> blackPawnMoves square board
    _                       -> []

allPossibleMoves :: Board -> Color -> [Move]
allPossibleMoves board color = concatMap (possibleMoves board) $ pieceSquares board color
