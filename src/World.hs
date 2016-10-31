module World where

import           Cell
import qualified Data.Vector                            as Vec
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Simulate (ViewPort)
import           Player

type Vec = Vec.Vector

-- | An index into the vector holding all the cells.
type Index = Int

-- | The x y coordinate of a cell.
type Coord = (Int, Int)

indexOfCoord :: World -> Coord -> Index
indexOfCoord world (x,y) = x + y * (width world)

coordOfIndex :: World -> Index -> Coord
coordOfIndex world i = (i `mod` width world, i `div` width world)

data AnimationSettings = Settings
    { simulationPeriod :: Float
    , windowSize       :: (Int, Int)
    , cellSize         :: Int
    , cellSpace        :: Int
    }

data World = World
    { cells  :: Vec Cell
    , width  :: Int
    , height :: Int
    }

data SimulationState = SimulationState
    { elapsedTime :: Float
    }

data GameState = GameState
    { world          :: World
    , settings       :: AnimationSettings
    , simulationData :: SimulationState
    , players        :: Vec Player
    }

mkState :: (Int, Int) -> GameState
mkState (width,height) =
    let cells = replicate (width * height) Unowned
        world =
            World
            { cells = Vec.fromList cells
            , width = width
            , height = height
            }
        simData =
            SimulationState
            { elapsedTime = 0
            }
        settings =
            Settings
            { simulationPeriod = 0.1
            , cellSize = 5
            , cellSpace = 1
            , windowSize = windowSizeFrom world settings
            }
    in GameState
       { world = world
       , simulationData = undefined
       , settings = settings
       , players = Vec.empty
       }

mkStateWithPlayer dim@(width,height) =
    let state = mkState dim
        w = world state
        cell = (cells w) Vec.! 0
        w' = setCell w (0,0) (Owned {player = dude})
        dude = mkPlayer name cellColor
    in state
       { players = Vec.singleton dude
       }
  where
    cellColor = makeColorI 0 76 153 1
    name = "dude"

-- | Convert a cell at a particular coordinate to a picture.
drawCell
    :: GameState -> Index -> Cell -> Picture
drawCell state@(GameState{world = world,settings = settings}) index cell =
    let cs = fromIntegral (cellSize settings)
        cp = fromIntegral (cellSpace settings)
        (x,y) = coordOfIndex world index
        fx = fromIntegral x * (cs + cp) + 1
        fy = fromIntegral y * (cs + cp) + 1
    in pictureOfCell (cellSize settings) fx fy cell

-- | Convert a world to a picture.
drawGame
    :: GameState -> Picture
drawGame state@(GameState{world = world,settings = settings}) =
    let (windowWidth,windowHeight) = windowSize settings
        offsetX = -fromIntegral windowWidth / 2
        offsetY = -fromIntegral windowHeight / 2
    in Translate offsetX offsetY $
       Pictures $ Vec.toList $ Vec.imap (drawCell state) (cells world)

simulateGame :: ViewPort -> Float -> GameState -> GameState
simulateGame _ time state = stepState state

-- | Get the cell at a particular coordinate in the world.
setCell
    :: World -> Coord -> Cell -> World
setCell world coord@(x,y) cell =
    world
    { cells = (cells world) Vec.// [(0, cell)]
    }

-- | Get the cell at a particular coordinate in the world.
getCell
    :: World -> Coord -> Cell
getCell world coord@(x,y)
  | x < 0 || x >= width world = Unowned
  | y < 0 || y >= height world = Unowned
  | otherwise = cells world Vec.! indexOfCoord world coord

-- | Get the neighbourhood of cells around this coordinate.
getNeighbourhood
    :: World -> Coord -> [Cell]
getNeighbourhood world (ix,iy) =
    let indexes =
            [ (x, y)
            | x <- [ix - 1 .. ix + 1]
            , y <- [iy - 1 .. iy + 1]
            , not (x == ix && y == iy) ]
    in map (getCell world) indexes

-- | Compute the next cell state depending on its neighbours.
stepCell
    :: Cell -> [Cell] -> Cell
stepCell cell neighbours =
  let playerNeighbours = map player $ filter (/= Unowned) neighbours
  in case playerNeighbours of
       []     -> cell
       (x:xs) -> cell {player = x}
  where
    dude = mkPlayer name cellColor
    cellColor = makeColorI 0 76 153 255
    name = "dude"

-- | Compute the next state of the cell at this index in the world.
stepIndex
    :: World -> Int -> Cell -> Cell
stepIndex world index cell =
    let coord = coordOfIndex world index
        neigh = getNeighbourhood world coord
    in stepCell cell neigh

stepState :: GameState -> GameState
stepState state@(GameState{world = world}) =
    let world' =
            world
            { cells = Vec.imap (stepIndex world) (cells world)
            }
    in state
       { world = world'
       }

-- Uninteresting utilities follow

-- | Get the size of the window needed to display a world.
windowSizeFrom
    :: World -> AnimationSettings -> (Int, Int)
windowSizeFrom world settings =
    let cSize = cellSize settings
        cSpace = cellSpace settings
        cPad = cSize + cSpace
        height' = cPad * (height world) + cSpace
        width' = cPad * (height world) + cSpace
    in (width', height')
