{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}

-- | The class that an overloaded string literal desugars to.
--
-- With OverloadedStrings a string literal becomes @fromString "..."@. The
-- compiler takes the method from the built-in scope, so the class must live
-- in the primitive package. @Data.String@ of @aihc-base@ exports it again.
module GHC.Prim.String
  ( IsString (..),
    eqString,
  )
where

import GHC.Prim (eqChar#)
import GHC.Prim.Base (String)
import GHC.Types (Bool (..), Char (..), isTrue#)

class IsString a where
  fromString :: String -> a

instance (a ~ Char) => IsString [a] where
  fromString string = string

-- | Whether two strings are equal. A string literal pattern of two or more
-- characters, without OverloadedStrings, compares with this function, as in
-- GHC. The compiler names it, so it lives in the primitive package.
eqString :: String -> String -> Bool
eqString [] [] = True
eqString (C# left : lefts) (C# right : rights) =
  case isTrue# (eqChar# left right) of
    True -> eqString lefts rights
    False -> False
eqString _ _ = False
