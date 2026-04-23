{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
module NonEmptyPattern where

import Data.List.NonEmpty (NonEmpty(..))

-- Three right-associative operators at the same precedence level
f (x :| y : z : ws) = undefined
f _ = undefined
