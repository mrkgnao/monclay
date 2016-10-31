module Player where

import           Graphics.Gloss hiding (color)

data Direction = R | U | L | D deriving (Eq, Show)

opposite :: Direction -> Direction
opposite R = L
opposite L = R
opposite U = D
opposite D = U

data Player = Player
    { name      :: String
    , riches    :: Integer
    , cellColor :: Color
    , direction :: Direction
    }
    deriving (Eq, Show)

mkPlayer :: String -> Color -> Direction -> Player
mkPlayer name color dir =
    Player
    { name = name
    , riches = 0
    , cellColor = color
    , direction = dir
    }


