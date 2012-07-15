module Lifter
       (
         World,
         Point,
         GameST,
         GameState (..),
         Direction (..),
         --
         row,
         col,
         robot,
         lambdas,
         playGame,
         move,
         distance,
         isPassable,
         isBlocked,
         isLiftOpen,
         newGameStateRandom,
         initGameState
       )
where       
  
import Prelude hiding (Either(..), catch)
import Data.Array.Unboxed  
import qualified Data.List as L (find, foldl', maximumBy)
import Data.Function (on)
import Data.Maybe
import Data.Time.Clock

import Control.Monad.State.Strict
import System.IO
import System.Random
import System.Exit
import System.Posix.Signals
import Control.DeepSeq

type Row = Int  
type Col = Int
type Point = (Row, Col)
type World = Array Point Tile

row :: Point -> Row
row = fst

col :: Point -> Col
col = snd

colBound :: World -> Col
colBound = col . snd . bounds

rowBound :: World -> Row
rowBound = row . snd . bounds


data Tile = Robot
          | Lambda
          | Rock
          | Wall
          | ClosedLift 
          | OpenLift             
          | Earth 
          | Space
          | READ_ERROR  
          deriving (Eq,Enum,Bounded)


data Direction = Up | Right | Down | Left | Wait | Abort deriving (Bounded, Eq, Enum, Ord)

instance Show Direction where
  show Up     = "U"
  show Right  = "R"
  show Down   = "D"
  show Left   = "L"
  show Wait   = "W"
  show Abort  = "A"


instance Show Tile where
  show Robot        = "R"
  show Lambda       = "\\"
  show Rock         = "*"
  show Wall   	    = "#"
  show ClosedLift   = "L"
  show OpenLift     = "O"
  show Space        = " "
  show Earth        = "."
  show _            = "ERROR"

data GameState = GameState
  { world :: !World
  , lambdasTotal :: !Int  
  , waterLevel :: !Int
  , flooding :: !Int    
  , waterproof :: !Int             
  , startTime :: !UTCTime
  , turns :: ![Direction]
  , rnd :: !StdGen  
  , liftPoint:: !Point  
  , bestScore :: !Int  
  , bestScoreOnTurn :: !Int  
  } 

instance NFData GameState

type GameST a = State GameState a

readTile :: Char -> Tile
readTile 'R'  = Robot
readTile '\\' = Lambda
readTile '*'  = Rock
readTile ' '  = Space
readTile '#'  = Wall
readTile '.'  = Earth
readTile 'L'  = ClosedLift
readTile 'O'  = OpenLift
readTile _    = READ_ERROR


distance :: Point -> Point -> Int
distance l1 l2 = let rowDist = abs $ (row l1) - (row l2)
                     colDist = abs $ (col l1) - (col l2)
                 in rowDist + colDist


-- | parses a line of the map and returns list of tiles or [] if the line doesn't contain tail info
parseLine :: String -> [Tile]
parseLine s = if elem READ_ERROR parseLine' then [] else parseLine'
  where parseLine' = map readTile s

rpad :: Int -> [Tile] -> [Tile]
rpad n t = if (length t) < n then rpad n (t ++ [Space]) else t

normalize :: [[Tile]] -> [[Tile]]
normalize xs = map (rpad maxLen) xs
  where  maxLen = length $ L.maximumBy (compare `on` length) xs



parseMap :: [String] -> [[Tile]]
parseMap  = reverse . normalize . validLines 
  where validLines = filter (\x-> x /= []) . map parseLine
        
 
parseWater :: [String] -> Int
parseWater _ =  0

parseFlooding :: [String] -> Int
parseFlooding _ =  0

parseWaterproof :: [String] -> Int
parseWaterproof _ =  0

isRobot :: Tile -> Bool
isRobot =  (==) Robot

isMineLift :: Tile -> Bool
isMineLift =  flip elem [ClosedLift, OpenLift]

isLiftOpen :: World -> Bool
isLiftOpen w = (w ! (mineLift w)) == OpenLift

isLambda :: Tile -> Bool
isLambda = (==) Lambda

robot :: World -> Point 
robot = fst . fromJust .  L.find (isRobot . snd) . assocs

isRobotAlive :: World -> World -> Bool 
isRobotAlive w wPrev = let pUp = move (robot w) Up
                           tileUp = w ! pUp
                           tileUpPrev = wPrev ! pUp
                       in not (tileUp == Rock && tileUpPrev /= Rock)
                    
isRobotAtOpenLift :: GameState -> Bool                    
isRobotAtOpenLift gs = (robot (world gs)) == (liftPoint gs)

mineLift :: World -> Point
mineLift = fst . fromJust .  L.find (isMineLift . snd) . assocs

lambdas :: World -> [Point]
lambdas = map fst . filter (isLambda . snd) . assocs


-- | returns true if robot is able to move to the given point
isPassable :: World -> Point -> Direction -> Bool
isPassable w p d
  | (d == Down) &&  ((w ! p2Up) == Rock) = False 
  | elem (w ! p) [Space, Earth, Lambda, OpenLift] = True
  | (w ! p) == Rock 
    && (d == Right || d == Left) 
    && (w ! (move p d)) == Space = True 
  | otherwise = False
  where p2Up = move (move p Up) Up

-- | returns true if robot is blocked at given point and not able to move anymore
isBlocked :: World -> Point -> Bool
isBlocked w p = (length $ map (isPassable w p ) [Left,Right,Up,Down]) == 0

-- | read map from standart input and create initial game state  
initGameState :: IO GameState
initGameState  = do
  time <- getCurrentTime
  gen <- newStdGen
  inputLines <- liftM (lines) getContents
  let listOfListOfTiles = parseMap inputLines
      rows = length listOfListOfTiles
      cols = length $ head listOfListOfTiles
      w = listArray ((1,1), (rows, cols)) $ concat listOfListOfTiles
      wtr = parseWater inputLines
      wtrprf = parseWaterproof inputLines      
      fld = parseFlooding inputLines
  return GameState {world = w, waterLevel = wtr, flooding = fld, lambdasTotal = length $ lambdas w,
                    waterproof = wtrprf, startTime = time, turns = [], rnd = gen, liftPoint = mineLift w,
                    bestScore = 0, bestScoreOnTurn = 0}

-- | returns string representation of the map
renderWorld :: World -> String
renderWorld w =   (unlines . reverse . lines . concatMap renderAssoc . assocs) w
  where
    maxCol = colBound w
    renderAssoc a 
      | col (fst a) == maxCol = show (snd a) ++ "\n"
      | otherwise = show (snd a)
                    
move :: Point -> Direction -> Point
move p Up    = (row p + 1, col p)
move p Down  = (row p - 1, col p)
move p Left  = (row p, col p - 1)
move p Right = (row p, col p + 1)
move p _     = p


moveRobot :: World -> Direction -> World                    
moveRobot w d = let curPos = robot w
                    newPos = move curPos d
                    canMove = curPos /= newPos && isPassable w newPos d
                    rockUpdate = if (w ! newPos) == Rock then [ ((move newPos d) , Rock) ] else []
                in if canMove 
                   then w // [(curPos, Space), (newPos, Robot)] // rockUpdate
                   else w
                    
-- | see section 2.3 of the manual                        
updateTile :: World -> Point -> Maybe [ (Point, Tile) ]
updateTile w p 
  -- rock is falling if cell under it is an empty space or a robot
  | (w ! p) == Rock 
    && (w ! pDown) == Space = Just [(p, Space), (pDown, Rock)]
  -- rock (x,y) is falling right down if there is a rock under it and (x+1,y),(x+1,y-1) are empty
  | (w ! p) == Rock 
    && (w ! pDown) == Rock
    && (w ! pRight) == Space
    && (w ! pRightDown) == Space = Just [(p, Space), (pRightDown, Rock)]
  -- rock (x,y) is falling left down if there is a rock under it and (x-1,y),(x-1,y-1) are empty 
  | (w ! p) == Rock 
    && (w ! pDown) == Rock
    && (w ! pLeft) == Space
    && (w ! pLeftDown) == Space = Just [(p, Space), (pLeftDown, Rock)]
  -- rocks are sliding right down on the lambdas                                           
  | (w ! p) == Rock 
    && (w ! pDown) == Lambda
    && (w ! pRight) == Space
    && (w ! pRightDown) == Space = Just [(p, Space), (pRightDown, Rock)] 
  -- lift is opened whe no lambda left
  | (w ! p) == ClosedLift && lambdas w == [] = Just [ (p, OpenLift) ]
  | otherwise = Nothing
  where pDown = move p Down
        pRight = move p Right
        pLeft = move p Left
        pUp = move p Up        
        pRightDown = move pDown Right
        pLeftDown = move pDown Left
        
  
  
-- | updating map according rules in 2.3 chapter of the manual
updateMap :: World -> World                        
updateMap w = let updates = concat $ catMaybes $ map (updateTile w) (indices w)
              in w // updates

-- | processes single robot move and returns transformed world according game rules 
processMove :: World -> Direction -> World
processMove w d = updateMap $ moveRobot w d

-- | evaluate single Robot move and update game state
updateGameState :: GameState -> Direction ->  GameState
updateGameState gs d = let newWorld = processMove (world gs) d
                           newWaterLevel = waterLevel gs
                       in GameState {world = newWorld, waterLevel = newWaterLevel, 
                                     flooding = flooding gs, waterproof = waterproof gs, 
                                     startTime = startTime gs, turns = d: (turns gs), 
                                     rnd = rnd gs, lambdasTotal = lambdasTotal gs, liftPoint = liftPoint gs,
                                     bestScore = bestScore gs, bestScoreOnTurn = bestScoreOnTurn gs}    
                          
updateGameStateBestScore:: GameState -> GameState                          
updateGameStateBestScore gs = let score = calcScore gs
                                  newBest = max score (bestScore gs)
                                  newBestOnTurn = if score > (bestScore gs) 
                                                  then (length $ turns gs) else bestScoreOnTurn gs
                              in GameState {world = world gs, waterLevel = waterLevel gs, flooding = flooding gs, 
                                            waterproof = waterproof gs, startTime = startTime gs, 
                                            turns = turns gs, rnd = rnd gs, lambdasTotal = lambdasTotal gs,
                                            liftPoint = liftPoint gs, bestScoreOnTurn = newBestOnTurn,
                                            bestScore = newBest}
                          
stripToBestTurn :: GameState -> GameState                                 
stripToBestTurn gs = GameState {world = world gs, waterLevel = waterLevel gs, flooding = flooding gs, 
                                            waterproof = waterproof gs, startTime = startTime gs, 
                                            turns = reverse $ take (bestScoreOnTurn gs) $ reverse $ turns gs, 
                                            rnd = rnd gs, lambdasTotal = lambdasTotal gs,
                                            liftPoint = liftPoint gs, bestScoreOnTurn = bestScoreOnTurn gs,
                                            bestScore = bestScore gs}

newGameStateRandom :: GameState -> StdGen -> GameState
newGameStateRandom gs newGen = GameState {world = world gs, waterLevel = waterLevel gs, flooding = flooding gs, 
                                          waterproof = waterproof gs, startTime = startTime gs, 
                                          turns = turns gs, rnd = newGen, lambdasTotal = lambdasTotal gs,
                                          liftPoint = liftPoint gs, bestScoreOnTurn = bestScoreOnTurn gs,
                                          bestScore = bestScore gs}



replayGameState :: GameState -> [Direction] ->  GameState
replayGameState gs0 = L.foldl' updateGameState gs0
                          
-- | returns notal number of lambdas collected by robot
lambdasCollected :: GameState -> Int
lambdasCollected gs = (lambdasTotal gs) - (length $ lambdas (world gs))

-- | calculate final score for given GS
calcScore :: GameState -> Int
calcScore gs = let turnsPoint = -(length $ turns gs)
                   modifier = if isRobotAtOpenLift gs then 75 else 50
               in turnsPoint + modifier * (lambdasCollected gs)

-- | compare to GameState and returns one with maximum score
getBestGameState :: GameState -> GameState -> GameState
getBestGameState gsP gsN = if (calcScore gsN) > (calcScore gsP) then gsN else gsP


printGameState :: GameState -> String
printGameState gs = "lambdas: " ++ (show $ lambdasCollected gs)
                    ++ " score: " ++ (show $ calcScore gs)
                    ++ " turns: " ++ (show $ reverse $ turns gs)
                    ++ "\n" ++ (renderWorld (world gs))


printResultAndQuit :: GameState -> GameState -> IO()    
printResultAndQuit gs0 gs = do putStrLn  $ printGameState gs
                               let steps = scanl updateGameState gs0 (reverse $ turns gs)
                               mapM_ (putStrLn . printGameState) steps
                               hFlush stdout
                               exitSuccess
    
checkEndCondition :: GameState -> GameState -> Bool
checkEndCondition gs gsPrev
  | (not $ isRobotAlive (world gs) (world gsPrev)) = True
  | (length $ turns gs) > quot ((colBound w) * (rowBound w) ) 2  = True
  | isRobotAtOpenLift gs = True
  | (head (turns gs)) == Abort = True
  | otherwise = False
  where w = world gs
                        
playOneGame :: GameState -> (Int -> GameST Direction) -> GameState     
playOneGame gs doTurn = let (d, newGsSt) = runState (doTurn 5) gs 
                            newGs = updateGameStateBestScore $ updateGameState newGsSt d
                        in if checkEndCondition newGs gs
                           then newGs
                           else playOneGame newGs doTurn

playGame :: GameState -> GameState -> (Int -> GameST Direction) -> IO()
playGame gs0 gsbest doTurn =  playGame' gs0 gsbest 0 doTurn

playGame' :: GameState -> GameState -> Int -> (Int -> GameST Direction) -> IO()
playGame' gs0 gsbest it doTurn = do let gs00 = if it > 10000 then gs0  else gs0
                                        lenGsBest = length $ turns gsbest 
                                        turnsGsBest = take (quot lenGsBest 2) (turns gsbest)
                                        endSt = stripToBestTurn $ playOneGame gs00 doTurn
                                        newBest = endSt `deepseq` getBestGameState gsbest endSt
                                    liftIO $  do if (turns newBest) /= (turns gsbest)
                                                   then putStrLn $ printGameState endSt 
                                                   else return()
                                                 s <- getPendingSignals
                                                 if s `seq` inSignalSet sigINT s
                                                   then printResultAndQuit gs0 newBest
                                                   else return()
                                    nb <- do return $!! newBest 
                                    newGen <- newStdGen         
                                    gs0newGen <- do return $!! newGameStateRandom gs0 newGen
                                    playGame' gs0newGen  nb (it + 1) doTurn
                             