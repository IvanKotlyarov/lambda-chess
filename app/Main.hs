module Main where

import Bot.Minimax
import Display
import Core
import Data.Char
import GHC.Conc (threadDelay)
import Bot.Human
import Bot.Random

play bot1 bot2 board color = do
    let bot = if color == White then bot1 else bot2
    printBoard board
    newBoard <- bot board color
    --printBoard newBoard
    --putStrLn $ show $ last (history newBoard)
    --threadDelay 100000
    if isMate newBoard (other color) then do
        printBoard newBoard
        putStrLn $ toPGN (history newBoard)
        putStrLn $ show color ++ " won"
    else
        if isDraw board then do
            putStrLn $ toPGN (history newBoard)
            putStrLn "Draw"
        else
            play bot1 bot2 newBoard (other color)

main :: IO ()
main = do
    let bot1 = humanMakeMove
    let bot2 = makeMinimaxMove
    play bot1 bot2 initialBoard White

    return ()
