module Language.Haskell.TH.Compat.Bang.Current (
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
  (Bang, BangType, VarBangType,)
import Language.Haskell.TH.Lib (BangQ)
import Language.Haskell.TH
  (bang, BangTypeQ, bangType, VarBangTypeQ, varBangType,
   SourceUnpackedness (..), SourceUnpackednessQ,
   SourceStrictness (..), SourceStrictnessQ,
   noSourceUnpackedness, sourceNoUnpack, sourceUnpack,
   noSourceStrictness, sourceLazy, sourceStrict, )
