
{-# LANGUAGE FlexibleInstances #-}

import Test.Benchmark.Function
import Data.RandomAccessList
import Control.Monad
import Data.Vector
import Data.Sequence
import Control.Applicative
import Data.Monoid
import Data.Bimap
import Control.Arrow

monoidFromApplicativeList xs = monoidFromList (Prelude.map pure xs) 

monoidFromList xs = Prelude.foldl mappend mempty xs

list = [1..10000]

instance Monoid (Bimap Int Integer) where
  mempty = Data.Bimap.empty
  mappend bm1 bm2 = Prelude.foldl (\subTotal (a,b) -> Data.Bimap.insert a b subTotal) bm1 (Data.Bimap.assocs bm2)

main = do 
  print "initialising data"
  let list' =  list
  print "initialising Data.List"
  runData list'
  print "initialising Data.List of Ints"
  let listInts = Prelude.map fromIntegral list'
  runData listInts
  print "initialising Data.Vector"
  let vector = monoidFromApplicativeList (Prelude.map fromIntegral list) :: Vector Integer
  runData vector
  print "initialising Data.Bimap"
  let bimap = monoidFromList (Prelude.map (fromIntegral >>= Data.Bimap.singleton) list)  :: Bimap Int Integer
  runData bimap
  print "initialising Data.RandomAccessList"
  let randomAccessList = Data.RandomAccessList.fromList list'
  runData randomAccessList
  print "initialising Data.Sequence"
  let sequen = monoidFromApplicativeList (Prelude.map fromIntegral list) :: Seq Integer
  runData sequen
  print "finish initialising data"
  putStrLn "testing Prelude.!! " 
  timeAndPrintAction (Control.Monad.forM listInts (\x -> runData ((Prelude.!!) list' (x-1))))
  putStrLn "testing Data.Vector.! " 
  timeAndPrintAction (Control.Monad.forM listInts (\x -> runData ((Data.Vector.!) vector (x-1))))
  putStrLn "testing Data.Bimap.! " 
  timeAndPrintAction (Control.Monad.forM listInts (\x -> runData ((Data.Bimap.!) bimap x)))
  putStrLn "testing Data.RandomAcessList.lookup " 
  timeAndPrintAction ((Control.Monad.forM listInts (\x -> runData (Data.RandomAccessList.lookup (x-1) randomAccessList))))
  putStrLn "testing Data.Sequence.index"
  timeAndPrintAction ((Control.Monad.forM listInts (\x -> runData (Data.Sequence.index sequen (x-1)))))
