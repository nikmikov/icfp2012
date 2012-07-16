module Main(main)
where

import Prelude hiding (Either(..), catch)  

import System.Random
import System.Posix.Signals
import Control.Monad.State.Strict
import Data.List (sortBy, elemIndex)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, isJust, fromJust)


import Lifter
   
data Path = Path {
    path :: [Direction]
  , startPoint :: Point
  , endPoint :: Point  
  } deriving (Eq, Show)
  
  
initPath :: Point -> Path                     
initPath pt = Path {path = [], startPoint = pt, endPoint = pt}
 
-- | return the destination Point of this Path
evalPath :: Path -> Point
evalPath  = endPoint

-- | return new Path by adding given direction
addDirectionToPath :: GameState -> Path -> Direction -> Path
addDirectionToPath gs p d = Path {
    path = (path p) ++ [d]
  , startPoint = startPoint p
  , endPoint = moveWithTrampolineCheck gs (endPoint p) d
  }


-- | return list of valid directions from the given point
validDirections :: GameState -> Point -> [Direction]
validDirections gs p = let w = world gs
                           isPass' d = isPassable w (move p d) d
                       in filter isPass'  [Up,Down,Left,Right] 

        
-- | similar to above function        
validPathsFrom :: GameState -> Path -> Set.Set Point -> [Path]        
validPathsFrom w p visited = map (addDirectionToPath w p) $ filter (filterVisited) $ validDirections w (evalPath p)
  where filterVisited d = Set.notMember (move (evalPath p) d) visited


-- | returns Path if one of the [Path] evaluates to the given Point
findPath :: Point -> [Path] -> Maybe Path
findPath pt pths 
  | null filterPaths = Nothing
  | otherwise = Just $ head filterPaths
  where filterPaths = filter (\a -> pt == (evalPath a) ) pths
  
  
-- | BFS seach implementation 
shortestPathBFS :: GameState
                   -> Int
                   -> Point
                   -> Point
                   -> [Path]
                   -> Set.Set Point -- optimisation to keep visited points in set
                   -> Maybe Path
shortestPathBFS w maxTryLength p1 p2 workList pset
  -- check if we are alredy at the destination
  | isJust $ findPath p2 workList = findPath p2 workList
  | null workList = Nothing
  -- check if path limit reached
  | (length $ path $ head workList) >= maxTryLength = Nothing 
  -- recursive call                                             
  | otherwise = shortestPathBFS w maxTryLength p1 p2 (fst genPaths) (snd genPaths)
  where genPaths =  foldl (foldFunc) ([], pset) workList
        foldFunc acc pth = ( (fst acc) ++ newPaths' , Set.union (snd acc) $ Set.fromList $ map (endPoint) newPaths' )
          where newPaths' = validPathsFrom w pth (snd acc)
  
-- | find shortest path to the given point if able to evaluate in less then calcMacDepth turns
shortestPathToPoint :: GameState -> Int -> Point -> Point -> Maybe Path
shortestPathToPoint w calcMaxDepth p1 p2 
  | p1 == p2 = Just (initPath p1)
  | otherwise = shortestPathBFS w calcMaxDepth p1 p2 [initPath p1] (Set.singleton p1)

  
pathsToLambdas :: GameState -> [Path]
pathsToLambdas gs = mapMaybe (sp') $ take 6 $ sortBy (compare') targets'
  where targets' = if isLiftOpen w then [liftPoint gs] else (lambdas w)++addRazors++addHorocks
        w = world gs
        addRazors = if (razorsAvail gs) < 2 then (razors w) else []
        addHorocks = getPassablePointsunderHorocks w
        --paths' = mapMaybe (sp') $ take 6 $ sortBy (compare') targets'
        compare' a b = compare (distance' a) (distance' b)
        distance' a = distance (robot w) a
        sp' = shortestPathToPoint gs calcMaxDepth (robot w)
        calcMaxDepth = 20
        
randomDirection ::StdGen ->  (Direction, StdGen)
randomDirection gen  = (directions !! randIdx, snd randResult)   
  where directions = [Up, Down, Left, Right]
        randIdx = fst randResult
        randResult = randomR (0::Int, 3::Int) gen 
 
doTurn' :: Int -> GameST Direction
doTurn' numTry = do gs <- get
                    let w  = world gs
                        cmp a b = compare (length $ path a) (length $ path b)
                        mainDirections = sortBy cmp $ pathsToLambdas gs
                        newD = if length mainDirections > 0
                               then head $ path $ head mainDirections
                               else Wait
                        rzrD = if ((razorsAvail gs) > 0) 
                                  && (length $ getGrowthAroundPoint w (robot w)) > 0
                               then ApplyRazor else newD
                        (dR, newGen) = if isBlocked w (robot w) || numTry == 0
                                       then (Abort, rnd gs)   
                                       else randomDirection (rnd gs)
                        randNum = (fromJust $ elemIndex dR [Up, Down, Left, Right]) `mod` 3 
                        d = if newD /= Wait && dR /= Abort then [newD, rzrD, dR]!!randNum else dR
                        p = move (robot w) d                        
                        newgs = newGameStateRandom gs newGen    
                    put newgs 
                    if (isPassable w p d) || (d == Abort) || (d == ApplyRazor)
                      then return d 
                      else doTurn' (numTry - 1)
                    


main :: IO ()
main = do  gs <- initGameState
           --putStrLn $ show $ waterLevel gs
           --putStrLn $ show $ flooding gs
           --putStrLn $ show $ waterproof gs
           --putStrLn $ show $ validDirections (world gs) (5,5)
           --putStrLn $ show $ shortestPathToPoint (world gs) 30 (robot (world gs)) (3,2)
           --putStrLn $ show $ (robot (world gs))
           --putStrLn $ show $ isPassable (world gs) (42,5) Left
           blockSignals (addSignal  sigINT emptySignalSet) 
           
           playGame gs gs doTurn'
           return ()
