{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module GHC.Types
  ( TYPE,
    Levity (..),
    RuntimeRep (..),
    VecCount (..),
    VecElem (..),
  )
where

data TYPE (rep :: RuntimeRep)

data RuntimeRep
  = AddrRep
  | BoxedRep Levity
  | DoubleRep
  | FloatRep
  | Int16Rep
  | Int32Rep
  | Int64Rep
  | Int8Rep
  | IntRep
  | SumRep [RuntimeRep]
  | TupleRep [RuntimeRep]
  | VecRep VecCount VecElem
  | Word16Rep
  | Word32Rep
  | Word64Rep
  | Word8Rep
  | WordRep

data Levity = Lifted | Unlifted

data VecCount = Vec16 | Vec2 | Vec32 | Vec4 | Vec64 | Vec8

data VecElem
  = DoubleElemRep
  | FloatElemRep
  | Int16ElemRep
  | Int32ElemRep
  | Int64ElemRep
  | Int8ElemRep
  | Word16ElemRep
  | Word32ElemRep
  | Word64ElemRep
  | Word8ElemRep
