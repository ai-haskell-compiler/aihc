{-|
    Module      :  Numeric.MixedType.Reduce
    Description :  Throw error when too inaccurate
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Mechanism to throw an error when a value gets too inaccurate.
-}
module Numeric.MixedTypes.Reduce
(
    CanGiveUpIfVeryInaccurate(giveUpIfVeryInaccurate)
    , numErrorVeryInaccurate
)
where

import Numeric.MixedTypes.PreludeHiding
-- import qualified Prelude as P

import Numeric.CollectErrors ( CN, NumError (NumError) )
import qualified Numeric.CollectErrors as CN

import Numeric.MixedTypes.Eq

class CanGiveUpIfVeryInaccurate t where
  {-| If the value contains so little information that it is seen as useless,
      drop the value and add an error indicating what happened.
    -}
  giveUpIfVeryInaccurate :: CN t -> CN t
  giveUpIfVeryInaccurate = id  -- by default, never give up!

numErrorVeryInaccurate :: String -> String -> NumError
numErrorVeryInaccurate context detail =
  case (context, detail) of
     ("", "") -> NumError $ msg <> "."
     ("", _) -> NumError $ msg <> ": " ++ detail
     (_, "") -> NumError $ context <> ": " <> msg <> "."
     _ -> NumError $ context <> ": " <> msg <> ": " ++ detail
  where
  msg = "Very inaccurate, too little information"

instance CanGiveUpIfVeryInaccurate Int
instance CanGiveUpIfVeryInaccurate Integer
instance CanGiveUpIfVeryInaccurate Rational
instance CanGiveUpIfVeryInaccurate Double where
  giveUpIfVeryInaccurate d
    | isFinite d = d
    | isNaN d = CN.prependErrorCertain (CN.NumError "NaN") d
    | otherwise = CN.prependErrorCertain (CN.NumError "Inifinity") d
