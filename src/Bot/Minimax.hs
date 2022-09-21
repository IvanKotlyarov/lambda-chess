module Bot.Minimax where

import Control.Monad.Random (getRandomR)

import Core
import Core (Board)

minimaxMove :: Board -> Color -> IO Move
minimaxMove board color = do
    return $ minimax board 3 color 

makeMinimaxMove :: Board -> Color -> IO Board
makeMinimaxMove board color = do
    move <- minimaxMove board color
    return $ movePiece move board
