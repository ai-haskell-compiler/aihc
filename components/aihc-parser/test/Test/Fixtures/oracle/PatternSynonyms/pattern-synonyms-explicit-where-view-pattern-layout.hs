{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module PatternSynonymsExplicitWhereViewPatternLayout where

pattern P r <- (T ((2 / pi *) -> r))
  where
    P r = T $ r * pi / 2
