module Bot.Random where

import Control.Monad.Random (getRandomR)

import Core
import Core (Board)

randomMove :: [Move] -> IO Move
randomMove moves = do
    randomIndex <- getRandomR (0, length moves - 1)
    return $ moves !! randomIndex

makeMove :: Board -> Color -> IO Board
makeMove board color = do
    let moves = allPossibleMoves board color
    move <- randomMove moves
    print move
    return $ movePiece move board
