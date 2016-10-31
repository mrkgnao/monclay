module Main where

import World
import Lib
import Graphics.Gloss

main :: IO ()
main = do
    let width = 150
    let height = 150
    let state = mkStateWithPlayer (width, height)
    play
        (InWindow
             "John Conway's Game of Life"
             (windowSize $ settings state)
             (5, 5))
        white
        10
        state
        drawGame
        eventHandler
        simulateGame
