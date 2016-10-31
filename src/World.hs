module World where

import           Cell
import qualified Data.Vector                        as Vec
import           Debug.Trace                        (trace)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
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
    , worldAge    :: Float
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
            , worldAge = 0
            }
        settings =
            Settings
            { simulationPeriod = 0.2
            , cellSize = 3
            , cellSpace = 1
            -- | Notice that this is a part of `settings` defined in terms of itself!
            -- | It is times like these that make me love Haskell <3
            , windowSize = windowSizeFrom world settings
            }
    in GameState
       { world = world
       , simulationData = simData
       , settings = settings
       , players = Vec.empty
       }

mkStateWithPlayer dim@(width,height) =
    let state = mkState dim
        w = world state
        w' =
            setCell
                w
                (20, 20)
                (Owned
                 { player = dude
                 })
        w'' =
            setCell
                w'
                (60, 60)
                (Owned
                 { player = nana
                 })
    in state
       { players = Vec.fromList [dude, nana]
       , world = w''
       }
  where
    cellColor = makeColorI 0 76 153 255
    cellColor' = makeColorI 204 0 102 255
    dude = mkPlayer "dude" cellColor U
    nana = mkPlayer "nana" cellColor' L

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

simulateGame :: Float -> GameState -> GameState
simulateGame time state = stepState time state

-- | Get the cell at a particular coordinate in the world.
setCell
    :: World -> Coord -> Cell -> World
setCell world coord@(x,y) cell =
    world
    { cells = (cells world) Vec.// [(indexOfCoord world coord, cell)]
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
    :: World -> Coord -> [(Direction, Cell)]
getNeighbourhood world (ix,iy) =
    let indexes = [(ix + 1, iy), (ix - 1, iy), (ix, iy + 1), (ix, iy - 1)]
        dirs = [R, L, U, D]
    in zipWith (,) dirs $ map (getCell world) indexes

-- | Compute the next cell state depending on its neighbours.
stepCell
    :: Cell -> [(Direction, Cell)] -> Cell
stepCell cell neighbours =
    case cell of
        PlayerOn{player = player} ->
            Owned
            { player = player
            }
        otherwise ->
            let cellNeighbours = filter ((/= Unowned) . snd) neighbours
            in case cellNeighbours of
                   [] -> cell
                   ((d,c):xs) ->
                       case directionMatchPlayers of
                           [] -> cell
                           (y:ys) ->
                               Owned
                               { player = player c
                               }
                       where directionMatchPlayers =
                                 filter
                                     (\(d',c') ->
                                           opposite d' == direction (player c'))
                                     cellNeighbours

-- | Compute the next state of the cell at this index in the world.
stepIndex
    :: World -> Int -> Cell -> Cell
stepIndex world index cell =
    let coord = coordOfIndex world index
        neigh = getNeighbourhood world coord
    in stepCell cell neigh

stepState :: Float -> GameState -> GameState
stepState time state@(GameState{settings = settings,simulationData = simData,world = world})
  | trace (show $ worldAge simData) False = undefined
  | elapsedTime simData >= simulationPeriod settings =
      let world' =
              world
              { cells = Vec.imap (stepIndex world) (cells world)
              }
          simData' =
              simData
              { elapsedTime = 0
              , worldAge = (worldAge simData) + 1
              }
      in state
         { world = world'
         , simulationData = simData'
         }
  | otherwise =
      let simData' =
              simData
              { elapsedTime = (elapsedTime simData) + time
              }
      in state
         { simulationData = simData'
         }

eventHandler :: Event -> GameState -> GameState

eventHandler (EventKey (Char 'r') _ _ _) state =
    mkStateWithPlayer (worldWidth, worldHeight)
  where
    w = world state
    worldWidth = width w
    worldHeight = height w

eventHandler (EventKey (Char 'q') _ _ _) state = error "user-initiated quit"

eventHandler evt state = state

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
