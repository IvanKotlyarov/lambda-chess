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
