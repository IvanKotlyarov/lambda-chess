{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Core where

import           Data.Maybe (fromJust, isJust)
import qualified Data.Map as M
import Debug.Trace

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

data Board = Board (M.Map Square Piece) (Maybe Square) Bool Bool Bool Bool [Move]
  deriving (Eq, Show)

findPiece :: Board -> Square -> Maybe Piece
findPiece (Board squares _ _ _ _ _ _) square = M.lookup square squares

squares :: Board -> M.Map Square Piece
squares board@(Board sqs _ _ _ _ _ _) = sqs
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
emptyBoard = Board M.empty Nothing False False False False []

initialBoard :: Board
initialBoard = Board (M.fromList (whitePawns ++ pieces White ++ blackPawns ++ pieces Black)) Nothing True True True True []
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
history (Board _ _ _ _ _ _ h) = h

isOnBoard :: Square -> Bool
isOnBoard (col, row)
  -- col `elem` cols && row `elem` rows
  -- a bit faster:
  = col >= 'a' && col <= 'h' && row >= 1 && row <= 8

placePiece :: Square -> Piece -> Board -> Board
placePiece square piece (Board squares enPassant wkc wqc bkc bqc h) = Board (M.insert square piece squares) enPassant wkc wqc bkc bqc h

deletePiece :: Square -> Board -> Board
deletePiece square (Board squares enPassant wkc wqc bkc bqc h) = Board (M.delete square squares) enPassant wkc wqc bkc bqc h

movePiece :: Move -> Board -> Board
movePiece m@(KingsideCastling White) (Board sqs _ _ _ kingSide queenSide h)
    = placePiece ('f', 1) (Piece Rook White)
    $ deletePiece ('h', 1)
    $ placePiece ('g', 1) (Piece King White)
    $ deletePiece ('e', 1) (Board sqs Nothing False False kingSide queenSide (h ++ [m]))

movePiece m@(KingsideCastling Black) (Board sqs _ kingSide queenSide _ _ h)
    = placePiece ('f', 8) (Piece Rook Black)
    $ deletePiece ('h', 8)
    $ placePiece ('g', 8) (Piece King Black)
    $ deletePiece ('e', 8) (Board sqs Nothing kingSide queenSide False False (h ++ [m]))

movePiece m@(QueensideCastling White) (Board sqs _ _ _ kingSide queenSide h)
    = placePiece ('d', 1) (Piece Rook White)
    $ deletePiece ('a', 1)
    $ placePiece ('c', 1) (Piece King White)
    $ deletePiece ('e', 1) (Board sqs Nothing False False kingSide queenSide (h ++ [m]))

movePiece m@(QueensideCastling Black) (Board sqs _ kingSide queenSide _ _ h)
    = placePiece ('d', 8) (Piece Rook Black)
    $ deletePiece ('a', 8)
    $ placePiece ('c', 8) (Piece King Black)
    $ deletePiece ('e', 8) (Board sqs Nothing kingSide queenSide False False (h ++ [m]))

movePiece m@(DoubleSquare from@(fromCol, fromRow) to@(toCol, _)) board@(Board _ _ wk wq bk bq h)
    = Board sqs (Just enPassant) wk wq bk bq (h ++ [m])
    where
        sqs = squares $ placePiece to (if fromRow == 2 then whitePawn else blackPawn)
            $ deletePiece from
            $ deletePiece (toCol, fromRow) board
        enPassant = if fromRow == 2 then (fromCol, succ fromRow) else (fromCol, pred fromRow)

movePiece m@(EnPassant piece@(Piece _ color) from@(_, fromRow) to@(toCol, _)) (Board sqs _ wk wq bk bq h)
    = placePiece to piece
    $ deletePiece from
    $ deletePiece (toCol, fromRow) (Board sqs Nothing wk wq bk bq (h ++ [m]))

movePiece m@(Move (Piece King White) from to) (Board sqs _ _ _ bk bq h)
    = placePiece to whiteKing $ deletePiece from (Board sqs Nothing False False bk bq (h ++ [m]))

movePiece m@(Move (Piece King Black) from to) (Board sqs _ wk wq _ _ h)
    = placePiece to blackKing $ deletePiece from (Board sqs Nothing wk wq False False (h ++ [m]))

movePiece m@(Move (Piece Rook White) ('a', 1) to) (Board sqs _ wk _ bk bq h)
    = placePiece to whiteRook $ deletePiece ('a', 1) (Board sqs Nothing wk False bk bq (h ++ [m]))

movePiece m@(Move (Piece Rook White) ('h', 1) to) (Board sqs _ _ wq bk bq h)
    = placePiece to whiteRook $ deletePiece ('h', 1) (Board sqs Nothing False wq bk bq (h ++ [m]))

movePiece m@(Move (Piece Rook Black) ('a', 8) to) (Board sqs _ wk wq bk _ h)
    = placePiece to blackRook $ deletePiece ('a', 8) (Board sqs Nothing wk wq bk False (h ++ [m]))

movePiece m@(Move (Piece Rook Black) ('h', 8) to) (Board sqs _ wk wq _ bq h)
    = placePiece to blackRook $ deletePiece ('h', 8) (Board sqs Nothing wk wq False bq (h ++ [m]))

movePiece m@(Move piece from to) (Board sqs _ wk wq bk bq h)
     = placePiece to piece $ deletePiece from (Board sqs Nothing wk wq bk bq (h ++ [m]))

movePiece m@(Capture piece from to) (Board sqs _ wk wq bk bq h)
    = placePiece to piece $ deletePiece from (Board sqs Nothing wk wq bk bq (h ++ [m]))

movePiece m@(Promotion from to piece) board@(Board sqs _ wk wq bk bq h) 
    = placePiece to piece $ deletePiece from (Board sqs Nothing wk wq bk bq (h ++ [m]))

movePiece m@(CapturePromotion from to piece) (Board sqs _ wk wq bk bq h)
     = placePiece to piece $ deletePiece from (Board sqs Nothing wk wq bk bq (h ++ [m]))

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
whitePawnMoves square@(col, row) board@(Board _ maybeEnPassant _ _ _ _ _)
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
blackPawnMoves square@(col, row) board@(Board _ maybeEnPassant _ _ _ _ _)
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
whiteKingSideCastling b@(Board sqs _ wk _ _ _ _)
        = wk && not (any (\s -> isTaken s b || s `elem` capturesSquares) [('e', 1), ('f', 1), ('g', 1)])
    where
        captures = piecesCaptures b Black
        capturesSquares = map toSquare captures

blackKingSideCastling :: Board -> Bool
blackKingSideCastling b@(Board sqs _ _ _ bk _ _)
        = bk && not (any (\s -> isTaken s b || s `elem` capturesSquares) [('e', 8), ('f', 8), ('g', 8)])
    where
        captures = piecesCaptures b White
        capturesSquares = map toSquare captures

whiteQueenSideCastling :: Board -> Bool
whiteQueenSideCastling b@(Board sqs _ _ wq _ _ _)
        = wq && not (any (\s -> isTaken s b || s `elem` capturesSquares) [('e', 1), ('d', 1), ('c', 1)])
    where
        captures = piecesCaptures b Black
        capturesSquares = map toSquare captures

blackQueenSideCastling :: Board -> Bool
blackQueenSideCastling b@(Board sqs _ _ _ _ bq _)
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
isTakenBy square color (Board squares _ _ _ _ _ _) = case M.lookup square squares of
    Nothing -> False
    Just (Piece _ c) -> color == c

pieceSquares :: Board -> Color -> [Square]
pieceSquares board@(Board squares _ _ _ _ _ _) color = map fst $ filter (\(_, Piece _ c) -> c == color) $ M.toList squares

possibleMoves :: Board -> Square -> [Move]
possibleMoves board@(Board squares _ _ _ _ _ _) square = case M.lookup square squares of
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
pieceCaptures board@(Board squares _ _ _ _ _ _) square = case M.lookup square squares of
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
kingSquare (Board squares _ _ _ _ _ _) color = fst $ head $ filter (\(s, p) -> p == Piece King color) $ M.toList squares

isCheck :: Board -> Color -> Bool
isCheck board color = square `elem` capturesSquares
    where
        square = kingSquare board color
        captures = piecesCaptures board (other color)
        capturesSquares = map toSquare captures

isMate :: Board -> Color -> Bool
isMate board color = null (allPossibleMoves board color) && isCheck board color

isDraw :: Board -> Bool
isDraw board = null (allPossibleMoves board White) || null (allPossibleMoves board Black) || (length psw == 1 && length psb == 1)
    where 
        psw = pieceSquares board White
        psb = pieceSquares board Black

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
