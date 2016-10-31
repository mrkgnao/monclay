module Cell where

import           Graphics.Gloss
import           Player

data Cell
    = Unowned
    | Owned { player :: Player}
    | OnTrail { player :: Player}
    | PlayerOn { player :: Player}
    deriving (Eq,Show)

-- | A default cell
mkCell :: Cell
mkCell = Unowned

-- | The basic shape of a cell.
cellShape
    :: Int -> Int -> Int -> Picture
cellShape cellSize posXi posYi =
    let cs = fromIntegral cellSize
        posX = fromIntegral posXi
        posY = fromIntegral posYi
        x1 = posX
        x2 = posX + cs
        y1 = posY
        y2 = posY + cs
    in Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

-- | Convert a cell to a picture, based on a primitive shape.
--      We pass the shape in to avoid recomputing it for each cell.
pictureOfCell
    :: Int -> Int -> Int -> Cell -> Picture
pictureOfCell cellSize posX posY cell =
  coloredSquare
  where
    coloredSquare = Color c (cellShape cellSize posX posY)
    c = case cell of
          Owned{player = player} -> cellColor player
          Unowned                -> greyN 0.8
