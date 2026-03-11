{-# language CPP #-}

module Data.Primitive.Unlifted.Type
  ( UnliftedType
  ) where

#if !MIN_VERSION_base(4,16,0)
import GHC.Exts (TYPE, RuntimeRep(UnliftedRep))
#else
import GHC.Exts (UnliftedType)
#endif

#if !MIN_VERSION_base(4,16,0)
type UnliftedType = TYPE 'UnliftedRep
#endif
