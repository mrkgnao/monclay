module Player where

import           Graphics.Gloss hiding (color)

data Direction = R | U | L | D deriving (Eq, Show)

data Player = Player
    { name      :: String
    , riches    :: Integer
    , cellColor :: Color
    , direction :: Direction
    }
    deriving (Eq, Show)

mkPlayer :: String -> Color -> Player
mkPlayer name color =
    Player
    { name = name
    , riches = 0
    , cellColor = color
    , direction = U
    }


