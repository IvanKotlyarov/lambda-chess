module Main where

import Bot.Random
import Display
import Core
import Data.Char
import GHC.Conc (threadDelay)

play bot1 bot2 board color = do
    let bot = if color == White then bot1 else bot2
    newBoard <- bot board color

    putStrLn $ chr 27 : "[2J"
    putStrLn $ chr 27 : "[;H"
    printBoard newBoard
    putStrLn $ show $ last (history newBoard)

    if isMate newBoard (other color) then do
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
    let bot1 = makeMove
    let bot2 = makeMove
    play bot1 bot2 initialBoard White

    return ()
