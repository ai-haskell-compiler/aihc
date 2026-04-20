{- ORACLE_TEST xfail Multi-line import with TypeFamilies -}
{-# LANGUAGE TypeFamilies #-}
import Generic.Data.Function.FoldMap.Constructor
  ( GFoldMapC(gFoldMapC)
  , GenericFoldMap(type GenericFoldMapM) )
