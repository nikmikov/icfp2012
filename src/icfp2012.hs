module Main(main)
where

import System.Environment

  
main :: IO ()
main = do args <- getArgs
          return()        
--          if "--tests" `elem` args
--              then Test.runTests
--              else progOpts args >>= run
