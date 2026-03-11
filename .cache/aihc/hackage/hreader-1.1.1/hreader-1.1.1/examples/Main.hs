module Main where

import Control.Monad.Base
import Control.Monad.HReader
import Data.HSet

type ListA = '[Int, Bool, Char]

type ListB = '[Int, Bool]

type MonadA m =
  ( MHRElemsConstraint m ListA
  , MonadBase IO m
  )

type MonadB m =
  ( MHRElemsConstraint m ListB
  , MonadBase IO m
  )

printInt
  :: (MonadBase IO m, MonadHReader m, HGettable (MHRElements m) Int)
  => m ()
printInt = do
  int <- hask
  liftBase $ print $ "int: " ++ show (int :: Int)

printBool
  :: (MonadBase IO m, MonadHReader m, HGettable (MHRElements m) Bool)
  => m ()
printBool = do
  b <- hask
  liftBase $ print $ "bool: " ++ show (b :: Bool)

printChar
  :: (MonadBase IO m, MonadHReader m, HGettable (MHRElements m) Char)
  => m ()
printChar = do
  c <- hask
  liftBase $ print $ "char: " ++ show (c :: Char)

monadA :: (MonadA m) => m ()
monadA = do
  liftBase $ print "monad A"
  printInt
  printBool
  printChar
  monadB

monadB :: (MonadB m) => m ()
monadB = do
  liftBase $ print "monad B"
  printInt
  printBool

runB :: (AllHGettable els ListB) => HSet els -> IO ()
runB h = do
  print "performing some IO actions around B"
  runHReaderT h monadB

runA :: (AllHGettable els ListA) => HSet els -> IO ()
runA h = do
  print "performing some IO actions around A"
  runHReaderT h monadA

runAinB :: (MonadB m) => m ()
runAinB = do
  liftBase $ print "running A in B"
  h <- askHSet
  let c = 'b'
      hb :: HSet ListB
      hb = subHSet h
      ha = HSCons c hb
  liftBase $ runA ha

runBinA :: (MonadA m) => m ()
runBinA = do
  liftBase $ print "running B in A"
  h <- askHSet
  liftBase $ runB h

main :: IO ()
main = do
  let
    ha :: HSet ListA
    ha = HSCons 10 $ HSCons True $ HSCons 'c' HSNil
    hb :: HSet ListB
    hb = subHSet ha
  runHReaderT ha monadA
  runHReaderT ha monadB
  runHReaderT hb monadB
  runHReaderT ha runBinA
  runHReaderT hb runAinB
