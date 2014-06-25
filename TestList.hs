
import TimeFunction
import Data.RandomAccessList
import Control.Monad
import Data.Vector
import Data.Sequence
import Control.Applicative
import Data.Monoid


makeMonoid xs = Prelude.foldl mappend mempty (Prelude.map pure xs) 

list = [1..10000]



main = do 
  print "initialising data"
  let list' = list
  print "initialising Data.List"
  runFunction list'
  print "initialising Data.Vector"
  let vector = makeMonoid (Prelude.map fromIntegral list) :: Vector Integer
  runFunction vector
  print "initialising Data.RandomAccessList"
  let randomAccessList = Data.RandomAccessList.fromList list
  runFunction randomAccessList
  print "initialising Data.Sequence"
  let sequen = makeMonoid (Prelude.map fromIntegral list) :: Seq Integer
  runFunction sequen
  print "finish initialising data"
  putStrLn "testing Data.List.(!!) " 
  timeAndPrintAction (Control.Monad.forM list' (\x -> runFunction (list' !! (x-1)) ))
  putStrLn "testing Data.Vector.(!) " 
  timeAndPrintAction ((Control.Monad.forM list' (\x -> runFunction (vector ! (x-1)))))
  putStrLn "testing Data.RandomAcessList.index " 
  timeAndPrintAction ((Control.Monad.forM list' (\x -> runFunction ( Data.RandomAccessList.index (x-1) randomAccessList))))
  putStrLn "testing Data.Sequence.index"
  timeAndPrintAction ((Control.Monad.forM list' (\x -> runFunction (Data.Sequence.index sequen (x-1)))))
