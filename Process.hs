module Process where

import System.Process
import Control.Exception
import System.IO
import System.Exit

run s = handle (fail . (show :: SomeException -> String )) $ do
  (ih,oh,eh,pid) <- runInteractiveCommand s
  so <- hGetContents oh
  se <- hGetContents eh
  hClose ih
  ex <- waitForProcess pid
  case ex of
    ExitFailure e   -> fail $ "Failed with status: " ++ show e 
    _  | not (null se) -> fail se
       | otherwise     -> return so


