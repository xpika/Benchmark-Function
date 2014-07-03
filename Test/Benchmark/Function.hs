module Test.Benchmark.Function where

import Data.IORef
import Text.Printf
import Data.Time
import System.IO
import Data.Char
import System.Random


runFunction f = do 
                randomChar <- randomIO
                if (last (show f)) == randomChar then return () else return ()

timeAction action = do
  t1 <- getCurrentTime
  g <- action
  t2 <- getCurrentTime
  let timeInUnits = (realToFrac $ diffUTCTime t2 t1 :: Float) 
  return timeInUnits 

timeFunction f = timeAction (runFunction f)

timeAndPrintAction action = do
  time <- timeAction action
  printf "%f\n" time

