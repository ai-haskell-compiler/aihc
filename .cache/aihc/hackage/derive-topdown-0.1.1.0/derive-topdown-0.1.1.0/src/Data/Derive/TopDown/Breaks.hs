-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.Break
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.Breaks (
    char, int, int8, int16, int32, int64, word, word8, 
    word16, word32, word64, primitives, state, realWorld
)
where

import Language.Haskell.TH.Syntax
-- | This module contains type names that might break the instance generations
-- | New versions of derive-topdown may add more types, so you should write
-- | import list

-- | @Char#@ type
char :: Name
char   = Name (mkOccName "Char#")   (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
-- | @Int#@ type
int :: Name
int    = Name (mkOccName "Int#")    (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
-- | @Int8#@ type
int8 :: Name
int8   = Name (mkOccName "Int8#")   (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
-- | @Int16#@ type
int16 :: Name
int16  = Name (mkOccName "Int16#")  (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
-- | @Int32#@ type
int32 :: Name
int32  = Name (mkOccName "Int32#")  (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
-- | @Int64#@ type
int64 :: Name
int64  = Name (mkOccName "Int64#")  (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
-- | @Word#@ type
word :: Name
word   = Name (mkOccName "Word#")   (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
-- | @Word8#@ type
word8 :: Name
word8  = Name (mkOccName "Word8#")  (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
-- | @Word16#@ type
word16 :: Name
word16 = Name (mkOccName "Word16#") (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
-- | @Word32#@ type
word32 :: Name
word32 = Name (mkOccName "Word32#") (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
-- | @Word64#@ type
word64 :: Name
word64 = Name (mkOccName "Word64#") (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))

-- | list of @Name@: @[char, int, int8, int16, int32, int64, word, word8, word16, word32, word64]@
primitives :: [Name]
primitives = [char, int, int8, int16, int32, int64, word, word8, word16, word32, word64]

-- | @GHC.Prim.State#@ type
state :: Name
state  = Name (mkOccName "State#") (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))

-- | @GHC.Prim.RealWorld#@ type
realWorld :: Name
realWorld  = Name (mkOccName "RealWorld#") (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))