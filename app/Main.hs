module Main where

import Core
import Bot.Random
import Display

play bot1 bot2 board color = do
    let bot = if color == White then bot1 else bot2
    newBoard <- bot board color

    printBoard newBoard

    play bot1 bot2 newBoard (other color) 

main :: IO ()
main = do
    let bot1 = makeMove
    let bot2 = makeMove
    play bot1 bot2 initialBoard White 

    return ()
