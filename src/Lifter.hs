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
         razors,
         playGame,
         move,
         distance,
         isPassable,
         isRobotDrowned,
         isBlocked,
         isLiftOpen,
         getGrowthAroundPoint,
         getPassablePointsunderHorocks,
         newGameStateRandom,
         initGameState
       )
where       
  
import Prelude hiding (Either(..), catch)
import Data.Array.Unboxed  
import qualified Data.List as L (find, foldl', maximumBy, isPrefixOf)
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
          | Trampoline Char  
          | Target Char  
          | Growth Int
          | Razor
          | Horock
          | READ_ERROR  
          deriving (Eq)


data Direction = Up | Right | Down | Left | Wait | Abort | ApplyRazor deriving (Bounded, Eq, Enum, Ord)

instance Show Direction where
  show Up         = "U"
  show Right      = "R"
  show Down       = "D"
  show Left       = "L"
  show Wait       = "W"  
  show Abort      = "A"
  show ApplyRazor = "S"


instance Show Tile where
  show Robot          = "R"
  show Lambda         = "\\"
  show Rock           = "*"
  show Wall   	      = "#"
  show ClosedLift     = "L"
  show OpenLift       = "O"
  show Space          = " "
  show Earth          = "."
  show (Growth _)     = "W"
  show Razor          = "!"
  show Horock         = "@"
  show (Trampoline c) = [c]
  show (Target c)     = [c]
  show _              = "ERROR"
  

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
  , turnUnderWater :: !Int  
  , bestScore :: !Int  
  , bestScoreOnTurn :: !Int  
  , razorsAvail :: !Int  
  , growthOrig :: !Tile  
  , trampolines :: ! [ (Point, Point) ]  --  trampoline -> target
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
readTile 'W'  = Growth 25
readTile '!'  = Razor
readTile '@'  = Horock
readTile c    = readExtra c

readExtra :: Char -> Tile
readExtra c
  | elem c ['A'..'I'] = Trampoline c
  | elem c ['1'..'9'] = Target c
  | otherwise = READ_ERROR


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
        
-- | parse String "key val" and return value       
getValFromPair :: String -> Int
getValFromPair s = read $ (words s)!!1

parseTocken :: String -> Int -> [String] -> Int
parseTocken key defVal xs = if isJust maybeTocken then getValFromPair $ fromJust maybeTocken else defVal
    where maybeTocken = L.find (L.isPrefixOf key) xs 

parseWater = parseTocken "Water"0

parseFlooding =  parseTocken "Flooding" 0

parseWaterproof =  parseTocken "Waterproof" 5

parseGrowth = parseTocken "Growth" 25

parseRazors = parseTocken "Razors" 0

isRobot :: Tile -> Bool
isRobot =  (==) Robot

isMineLift :: Tile -> Bool
isMineLift =  flip elem [ClosedLift, OpenLift]

isLiftOpen :: World -> Bool
isLiftOpen w = (w ! (mineLift w)) == OpenLift

isLambda :: Tile -> Bool
isLambda = (==) Lambda

isRazor :: Tile -> Bool
isRazor = (==) Razor

isRock :: Tile -> Bool
isRock t = elem t [Rock, Horock]

robot :: World -> Point 
robot = fst . fromJust .  L.find (isRobot . snd) . assocs

isRobotAlive :: GameState -> GameState -> Bool 
isRobotAlive gs gsPrev = let w = world gs
                             wPrev = world gsPrev
                             pUp = move (robot w) Up
                             tileUp = w ! pUp
                             tileUpPrev = wPrev ! pUp
                         in not (isRock tileUp && not (isRock tileUpPrev) )
                            && not (isRobotDrowned gs)
                    
isRobotDrowned :: GameState -> Bool                            
isRobotDrowned gs = ((turnUnderWater gs) > (waterproof gs))
                            
isRobotAtOpenLift :: GameState -> Bool                    
isRobotAtOpenLift gs = (robot (world gs)) == (liftPoint gs)

mineLift :: World -> Point
mineLift = fst . fromJust .  L.find (isMineLift . snd) . assocs

lambdas :: World -> [Point]
lambdas = map fst . filter (isLambda . snd) . assocs

horocks :: World -> [Point]
horocks = map fst . filter ( (==) Horock . snd) . assocs


razors :: World -> [Point]
razors = map fst . filter (isRazor . snd) . assocs

trampolinesPts :: World -> [Point]
trampolinesPts = map fst . filter (isTrampoline . snd) . assocs

targetsPts :: World -> [Point]
targetsPts = map fst . filter (isTarget . snd) . assocs


isTrampoline :: Tile -> Bool
isTrampoline  (Trampoline _) = True
isTrampoline _ = False

isTarget :: Tile -> Bool
isTarget  (Target _) = True
isTarget _ = False

isGrowth :: Tile -> Bool
isGrowth  (Growth _) = True
isGrowth _ = False


-- | returns true if robot is able to move to the given point
isPassable :: World -> Point -> Direction -> Bool
isPassable w p d
  | (d == Down) &&  isRock ((w ! p2Up)) = False 
  | elem (w ! p) [Space, Earth, Lambda, OpenLift, Razor] = True
  | isTrampoline (w ! p)  = True
  | isRock (w ! p)
    && (d == Right || d == Left) 
    && (w ! (move p d)) == Space = True 
  | otherwise = False
  where p2Up = move (move p Up) Up

-- | returns true if robot is blocked at given point and not able to move anymore
isBlocked :: World -> Point -> Bool
isBlocked w p = (length $ map (isPassable w p ) [Left,Right,Up,Down]) == 0

-- | parse input lines and returns list of pair ('A','1')
parseTrampolines :: [String] -> [ (Char, Char) ]
parseTrampolines  = map (parse' . words) . filter (L.isPrefixOf "Trampoline")
  where parse' xs = ( head (xs!!1) , head (xs!!3) )  

findTrampl :: Char -> World -> Point
findTrampl c = fst . fromJust . L.find ( (==) (Trampoline c) . snd ) . assocs

findTarget :: Char -> World -> Point
findTarget c = fst . fromJust . L.find ( (==) (Target c) . snd ) . assocs


-- | return list of trampolines (Trampoline -> Target)
buildTrampolines :: World -> [String] -> [ (Point, Point) ]
buildTrampolines w = map trans' . parseTrampolines 
  where trans' (t1, t2) = (findTrampl t1 w, findTarget t2 w) 

-- | replace growth with given value
replaceGrowth :: Tile -> [Tile] -> [Tile]
replaceGrowth t = map (\x-> if isGrowth x then t else x ) 

-- | read map from standart input and create initial game state  
initGameState :: IO GameState
initGameState  = do
  time <- getCurrentTime
  gen <- newStdGen
  inputLines <- liftM (lines) getContents
  let listOfListOfTiles = parseMap inputLines
      rows = length listOfListOfTiles
      cols = length $ head listOfListOfTiles
      grwth = Growth ((parseGrowth inputLines) - 1)
      w = listArray ((1,1), (rows, cols)) $ replaceGrowth grwth $ concat listOfListOfTiles
  return GameState {world = w, waterLevel = parseWater inputLines, 
                    flooding = parseFlooding inputLines, 
                    lambdasTotal = length $ lambdas w,
                    waterproof = parseWaterproof inputLines, 
                    startTime = time, turns = [], rnd = gen, liftPoint = mineLift w,
                    bestScore = 0, bestScoreOnTurn = 0, turnUnderWater = 0, 
                    trampolines = buildTrampolines w inputLines,
                    razorsAvail = parseRazors inputLines, growthOrig = grwth}

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

findTargetPoint ::  Point -> GameState ->  Point
findTargetPoint p = snd . fromJust . L.find ( (==) p . fst )  . trampolines 

findTrampolinesForTarget :: Point -> GameState -> [Point]
findTrampolinesForTarget p = map fst . filter ( (==) p . snd )  . trampolines

moveRobotOnTrampoline :: GameState -> Point -> [ (Point, Tile) ]
moveRobotOnTrampoline gs p = (p, Space) : (moveTo', Robot): removedTrampolines'
  where moveTo' = findTargetPoint p gs
        removedTrampolines' = map (\x-> (x, Space) ) $ findTrampolinesForTarget moveTo' gs


moveRobot :: GameState -> Direction -> World                    
moveRobot gs d = let w = world gs
                     curPos = robot w
                     newPos = move curPos d
                     canMove = (curPos /= newPos && isPassable w newPos d) || d == ApplyRazor 
                     rockUpdate = if isRock (w ! newPos) then [ ((move newPos d) , (w ! newPos)) ] else []
                     tramplUpd =  if isTrampoline (w ! newPos)  then moveRobotOnTrampoline gs newPos else []
                     growthUpd = if d == ApplyRazor && (razorsAvail gs) > 0
                                 then applyRazorAtPoint w curPos else []
                 in if canMove 
                    then w // [(curPos, Space), (newPos, Robot)] // rockUpdate // tramplUpd // growthUpd
                    else w
                         
decGrowth (Growth 0) orig = orig
decGrowth (Growth n) _    = Growth (n -1) 
decGrowth t _ = t


-- | return list of empty cells surrounding given point
getEmptySurrounding :: World -> Point -> [Point]
getEmptySurrounding w p = filter ( (==) Space . (!) w ) [ (x1,y1)| x1<- [x-1..x+1], y1<- [y-1..y+1] ]
  where x = row p
        y = col p

-- | return list of empty cells surrounding given point
applyRazorAtPoint :: World -> Point -> [ (Point, Tile) ]
applyRazorAtPoint w p = map (\x-> (x, Space)) $ getGrowthAroundPoint w p
        
getGrowthAroundPoint :: World -> Point -> [Point]
getGrowthAroundPoint w p = filter ( isGrowth . (!) w ) [ (x1,y1)| x1<- [x-1..x+1], y1<- [y-1..y+1] ]
  where x = row p
        y = col p

getPassablePointsunderHorocks :: World -> [Point]
getPassablePointsunderHorocks w = filter (\x-> elem (w!x) [Space,Earth,Lambda,Razor] ) 
                                  $  map (flip move Down) $ horocks w

growGrowth :: World -> Point -> Tile -> [ (Point, Tile) ]  
growGrowth w p grOrig                  
  | (w ! p) == (Growth 0) = (p, grOrig ):( map (\x-> (x, grOrig) ) $ getEmptySurrounding w p)
  | isGrowth (w ! p) = ( p, (decGrowth (w!p) grOrig) ) : []
  | otherwise = []
              
-- | see section 2.3 of the manual                        
updateTile :: GameState -> World -> Point -> Maybe [ (Point, Tile) ]
updateTile gs w p 
  -- rock is falling if cell under it is an empty space or a robot
  | isRock (w ! p)
    && (w ! pDown) == Space = Just [(p, Space), (pDown, (cTile pDown) )]
  -- rock (x,y) is falling right down if there is a rock under it and (x+1,y),(x+1,y-1) are empty
  | isRock (w ! p)
    && isRock (w ! pDown)
    && (w ! pRight) == Space
    && (w ! pRightDown) == Space = Just [(p, Space), (pRightDown, (cTile pRightDown) )]
  -- rock (x,y) is falling left down if there is a rock under it and (x-1,y),(x-1,y-1) are empty 
  | isRock (w ! p)
    && isRock (w ! pDown) 
    && (w ! pLeft) == Space
    && (w ! pLeftDown) == Space = Just [(p, Space), (pLeftDown, (cTile pLeftDown) )]
  -- rocks are sliding right down on the lambdas                                           
  | isRock (w ! p) 
    && (w ! pDown) == Lambda
    && (w ! pRight) == Space
    && (w ! pRightDown) == Space = Just [(p, Space), (pRightDown, (cTile pRightDown) )] 
  -- lift is opened whe no lambda left
  | (w ! p) == ClosedLift && lambdas w == [] && horocks w == [] = Just [ (p, OpenLift) ]
  -- growth                                               
  | isGrowth (w ! p) = Just (growGrowth w p (growthOrig gs))
  | otherwise = Nothing
  where pDown = move p Down
        pRight = move p Right
        pLeft = move p Left
        pUp = move p Up        
        pRightDown = move pDown Right
        pLeftDown = move pDown Left
        cTile newPoint
          | (w ! p) == Rock = (w ! p)
          -- crashing horocks                    
          | (w ! p) == Horock = let  downTile = w ! (move newPoint Down) 
                                in if downTile /=Space then Lambda else (w ! p)
                                                       
  
  
-- | updating map according rules in 2.3 chapter of the manual
updateMap :: GameState -> World -> World                        
updateMap gs w = let updates = concat $ catMaybes $ map (updateTile gs w) (indices w)
                 in w // updates

-- | processes single robot move and returns transformed world according game rules 
processMove :: GameState -> Direction -> World
processMove gs d = updateMap gs (moveRobot gs d)

-- | evaluate single Robot move and update game state
updateGameState :: GameState -> Direction ->  GameState
updateGameState gs d = let newWorld = processMove gs d
                           turnsNum = length $ turns gs
                           floodInc = if (flooding gs) > 0 && (turnsNum `mod` (flooding gs) == 0) then 1 else 0
                           newWaterLevel = (waterLevel gs) + floodInc
                           r = robot $ world gs
                           isUnderWater = (row r) < (waterLevel gs)
                           newTurnsUnderWater = if isUnderWater then (turnUnderWater gs) + 1 else 0
                           razorsInc = if  ((world gs) ! (robot newWorld)) == Razor then 1 else 0 
                           razorsDec = if d == ApplyRazor then 1 else 0
                       in GameState {world = newWorld, waterLevel = newWaterLevel, 
                                     flooding = flooding gs, waterproof = waterproof gs, 
                                     startTime = startTime gs, turns = d: (turns gs), 
                                     rnd = rnd gs, lambdasTotal = lambdasTotal gs, liftPoint = liftPoint gs,
                                     bestScore = bestScore gs, bestScoreOnTurn = bestScoreOnTurn gs,
                                     turnUnderWater = newTurnsUnderWater, trampolines = trampolines gs,
                                     razorsAvail = (razorsAvail gs) + razorsInc - razorsDec, 
                                     growthOrig = growthOrig gs}    
                          
updateGameStateBestScore:: GameState -> GameState                          
updateGameStateBestScore gs = let score = calcScore gs
                                  newBest = max score (bestScore gs)
                                  newBestOnTurn = if score > (bestScore gs) 
                                                  then (length $ turns gs) else bestScoreOnTurn gs
                              in GameState {world = world gs, waterLevel = waterLevel gs, flooding = flooding gs, 
                                            waterproof = waterproof gs, startTime = startTime gs, 
                                            turns = turns gs, rnd = rnd gs, lambdasTotal = lambdasTotal gs,
                                            liftPoint = liftPoint gs, bestScoreOnTurn = newBestOnTurn,
                                            bestScore = newBest, turnUnderWater = turnUnderWater gs,
                                            trampolines = trampolines gs, razorsAvail = razorsAvail gs,
                                            growthOrig = growthOrig gs}
                          
stripToBestTurn :: GameState -> GameState                                 
stripToBestTurn gs = GameState {world = world gs, waterLevel = waterLevel gs, flooding = flooding gs, 
                                            waterproof = waterproof gs, startTime = startTime gs, 
                                            turns = reverse $ take (bestScoreOnTurn gs) $ reverse $ turns gs, 
                                            rnd = rnd gs, lambdasTotal = lambdasTotal gs,
                                            liftPoint = liftPoint gs, bestScoreOnTurn = bestScoreOnTurn gs,
                                            bestScore = bestScore gs, turnUnderWater = turnUnderWater gs,
                                            trampolines = trampolines gs,razorsAvail = razorsAvail gs,
                                            growthOrig = growthOrig gs}

newGameStateRandom :: GameState -> StdGen -> GameState
newGameStateRandom gs newGen = GameState {world = world gs, waterLevel = waterLevel gs, flooding = flooding gs, 
                                          waterproof = waterproof gs, startTime = startTime gs, 
                                          turns = turns gs, rnd = newGen, lambdasTotal = lambdasTotal gs,
                                          liftPoint = liftPoint gs, bestScoreOnTurn = bestScoreOnTurn gs,
                                          bestScore = bestScore gs, turnUnderWater = turnUnderWater gs,
                                          trampolines = trampolines gs, razorsAvail = razorsAvail gs,
                                          growthOrig = growthOrig gs}



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
                    ++ " razors: " ++ (show $ razorsAvail gs)                    
                    ++ "\n" ++ (renderWorld (world gs))


printResultAndQuit :: GameState -> GameState -> IO()    
printResultAndQuit gs0 gs = do putStrLn  $ printGameState gs
                               let steps = scanl updateGameState gs0 (reverse $ turns gs)
                               mapM_ (putStrLn . printGameState) steps
                               hFlush stdout
                               exitSuccess
    
checkEndCondition :: GameState -> GameState -> Bool
checkEndCondition gs gsPrev
  | (not $ isRobotAlive gs gsPrev) = True
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
                                                 -- if elem ApplyRazor (turns endSt) 
                                                 --  then putStrLn $ printGameState endSt        
                                                 --  else return()
                                    nb <- do return $!! newBest 
                                    newGen <- newStdGen         
                                    gs0newGen <- do return $!! newGameStateRandom gs0 newGen
                                    playGame' gs0newGen  nb (it + 1) doTurn
                             