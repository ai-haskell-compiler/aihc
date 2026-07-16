{-# LANGUAGE OverloadedStrings #-}

-- | Stable identities for runtime primitives.
--
-- Textual foreign-import names are resolved to these identities before
-- runtime-explicit lowering.  Backends must dispatch on 'PrimOp', not on
-- source text.
module Aihc.Tc.Prim
  ( PrimOp (..),
    primOpArity,
    primOpFromName,
    primOpName,
    SchedulerPrimOp (..),
    schedulerPrimOp,
  )
where

import Data.Text (Text)

data PrimOp
  = PrimIntAdd
  | PrimIntSubtract
  | PrimIntMultiply
  | PrimCompareInt
  | PrimIntLessThan
  | PrimIntEqual
  | PrimCharToInt
  | PrimIntToChar
  | PrimRaise
  | PrimRealWorld
  | PrimCatch
  | PrimNewMutVar
  | PrimReadMutVar
  | PrimWriteMutVar
  | PrimFork
  | PrimYield
  | PrimNewMVar
  | PrimTakeMVar
  | PrimPutMVar
  | PrimDelay
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

data SchedulerPrimOp
  = SchedulerFork
  | SchedulerYield
  | SchedulerNewMVar
  | SchedulerTakeMVar
  | SchedulerPutMVar
  | SchedulerDelay
  deriving (Eq, Ord, Read, Show)

primOpName :: PrimOp -> Text
primOpName primOp =
  case primOp of
    PrimIntAdd -> "+#"
    PrimIntSubtract -> "-#"
    PrimIntMultiply -> "*#"
    PrimCompareInt -> "compareInt#"
    PrimIntLessThan -> "<#"
    PrimIntEqual -> "==#"
    PrimCharToInt -> "charToInt#"
    PrimIntToChar -> "intToChar#"
    PrimRaise -> "raise#"
    PrimRealWorld -> "realWorld#"
    PrimCatch -> "catch#"
    PrimNewMutVar -> "newMutVar#"
    PrimReadMutVar -> "readMutVar#"
    PrimWriteMutVar -> "writeMutVar#"
    PrimFork -> "fork#"
    PrimYield -> "yield#"
    PrimNewMVar -> "newMVar#"
    PrimTakeMVar -> "takeMVar#"
    PrimPutMVar -> "putMVar#"
    PrimDelay -> "delay#"

primOpFromName :: Text -> Maybe PrimOp
primOpFromName name =
  lookup name [(primOpName primOp, primOp) | primOp <- [minBound .. maxBound]]

primOpArity :: PrimOp -> Int
primOpArity primOp =
  case primOp of
    PrimRealWorld -> 0
    PrimCharToInt -> 1
    PrimIntToChar -> 1
    PrimRaise -> 1
    PrimYield -> 1
    PrimNewMVar -> 1
    PrimIntAdd -> 2
    PrimIntSubtract -> 2
    PrimIntMultiply -> 2
    PrimCompareInt -> 2
    PrimIntLessThan -> 2
    PrimIntEqual -> 2
    PrimNewMutVar -> 2
    PrimReadMutVar -> 2
    PrimFork -> 2
    PrimTakeMVar -> 2
    PrimDelay -> 2
    PrimCatch -> 3
    PrimWriteMutVar -> 3
    PrimPutMVar -> 3

schedulerPrimOp :: PrimOp -> Maybe SchedulerPrimOp
schedulerPrimOp primOp =
  case primOp of
    PrimFork -> Just SchedulerFork
    PrimYield -> Just SchedulerYield
    PrimNewMVar -> Just SchedulerNewMVar
    PrimTakeMVar -> Just SchedulerTakeMVar
    PrimPutMVar -> Just SchedulerPutMVar
    PrimDelay -> Just SchedulerDelay
    _ -> Nothing
