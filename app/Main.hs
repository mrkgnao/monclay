module Main where

import World
import Lib
import Graphics.Gloss

main :: IO ()
main = do
    let width = 80
    let height = 60
    let state = mkState (width, height)
    simulate
        (InWindow
             "John Conway's Game of Life"
             (windowSize $ settings state)
             (5, 5))
        white
        10
        state
        drawGame
        simulateGame
