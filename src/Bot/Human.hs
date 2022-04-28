module Bot.Human where

import Control.Monad.Random (getRandomR)

import Core
import Core (Board)

humanMove :: Board -> Color -> IO Move
humanMove board color = do
    putStr "Type your move: "
    
    moveStr <- getLine
    case parseMove moveStr color of
        Just move -> if move `elem` allPossibleMoves board color then return move else humanMove board color
        Nothing -> humanMove board color

humanMakeMove :: Board -> Color -> IO Board
humanMakeMove board color = do
    move <- humanMove board color
    return $ movePiece move board
