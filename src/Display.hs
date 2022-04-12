module Display where

import qualified Data.Map as M
import Core
import Data.Foldable (traverse_)

printBoard :: Board -> IO ()
printBoard board = do
    mapM_ (printRow board) [8, 7 .. 1]

printRow :: Board -> Int -> IO ()
printRow board row = do
    traverse_ (\c -> printSquare board (c, row)) cols
    putStrLn ""

printSquare :: Board -> Square -> IO ()
printSquare (Board squares _ _ _ _ _ _) square =
    case M.lookup square squares of
        Just p -> putStr $ unicode p
        _      -> putStr " "

unicode :: Piece -> String
unicode (Piece Rook White) = "♖"
unicode (Piece Knight White) = "♘"
unicode (Piece Bishop White) = "♗"
unicode (Piece Queen White)  = "♕"
unicode (Piece King White)  = "♔"
unicode (Piece Pawn White) = "♙"

unicode (Piece Rook Black) = "♜"
unicode (Piece Knight Black) = "♞"
unicode (Piece Bishop Black) = "♝"
unicode (Piece Queen Black) = "♛"
unicode (Piece King Black) = "♚"
unicode (Piece Pawn Black) = "♟"