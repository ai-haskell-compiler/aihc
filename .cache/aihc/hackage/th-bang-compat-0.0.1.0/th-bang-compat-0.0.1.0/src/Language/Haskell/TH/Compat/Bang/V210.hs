module Language.Haskell.TH.Compat.Bang.V210 (
  Bang, BangQ,
  BangType, BangTypeQ,
  VarBangType, VarBangTypeQ,

  bang, bangType, varBangType,

  SourceUnpackedness (..), SourceUnpackednessQ,
  SourceStrictness (..), SourceStrictnessQ,

  noSourceUnpackedness, sourceNoUnpack, sourceUnpack,
  noSourceStrictness, sourceLazy, sourceStrict,
  ) where

import Language.Haskell.TH.Syntax
  (Strict, StrictType, VarStrictType)
import Language.Haskell.TH.Lib (unpacked)
import Language.Haskell.TH
  (Q, TypeQ, Name,
   notStrict, isStrict, strictType, varStrictType)

type Bang = Strict
type BangQ = Q Bang

type BangType = StrictType
type BangTypeQ = Q BangType

type VarBangType = VarStrictType
type VarBangTypeQ = Q VarBangType

type SourceStrictnessQ = Q SourceStrictness
type SourceUnpackednessQ = Q SourceUnpackedness

bang :: SourceUnpackednessQ -> SourceStrictnessQ -> BangQ
bang qu qs =
  do u <- qu
     s <- qs
     dispatch u s
  where
    dispatch :: SourceUnpackedness -> SourceStrictness -> BangQ
    dispatch NoSourceUnpackedness NoSourceStrictness = notStrict
    dispatch NoSourceUnpackedness SourceLazy         = fail "SourceLazy is not supported in this version of template-haskell."
    dispatch NoSourceUnpackedness SourceStrict       = isStrict
    dispatch SourceNoUnpack       _                  = fail "SourceNoUnpack is not supported in this version of template-haskell."
    dispatch SourceUnpack         _                  = unpacked

bangType :: BangQ -> TypeQ -> BangTypeQ
bangType = strictType

varBangType :: Name -> BangTypeQ -> VarBangTypeQ
varBangType = varStrictType

-----
-- fake constructors for `bang` interface

data SourceUnpackedness
  = NoSourceUnpackedness
  | SourceNoUnpack
  | SourceUnpack
  deriving (Show, Eq, Ord)

noSourceUnpackedness, sourceNoUnpack, sourceUnpack :: SourceUnpackednessQ

noSourceUnpackedness = return NoSourceUnpackedness
sourceNoUnpack = return SourceNoUnpack
sourceUnpack = return SourceUnpack

data SourceStrictness
  = NoSourceStrictness
  | SourceLazy
  | SourceStrict
  deriving (Show, Eq, Ord)

noSourceStrictness, sourceLazy, sourceStrict :: SourceStrictnessQ

noSourceStrictness = return NoSourceStrictness
sourceLazy = return SourceLazy
sourceStrict = return SourceStrict
