{-# LANGUAGE Trustworthy #-}
-- | Boolean functions.
--
-- Type families are defined in "Data.Type.Bool" module in @base@ package.
-- For term-level reflections see [defun-bool package](https://hackage.haskell.org/package/defun-bool).
--
module DeFun.Bool (
    -- * Logical and
    LAnd, LAndSym, LAndSym1,
    -- * Logical or
    LOr, LOrSym, LOrSym1,
    -- * Logical not
    Not, NotSym,
) where

import Data.Type.Bool (Not, type (&&), type (||))
import Prelude        (Bool)

import DeFun.Core

-------------------------------------------------------------------------------
-- LAnd
-------------------------------------------------------------------------------

-- | Logical and. A synonym of 'Data.Type.Bool.&&'
type LAnd :: Bool -> Bool -> Bool
type LAnd x y = x && y

type LAndSym :: Bool ~> Bool ~> Bool
data LAndSym x
type instance App LAndSym x = LAndSym1 x

type LAndSym1 :: Bool -> Bool ~> Bool
data LAndSym1 x y
type instance App (LAndSym1 x) y = LAnd x y

-------------------------------------------------------------------------------
-- LOr
-------------------------------------------------------------------------------

-- | Logical or. A synonym of 'Data.Type.Bool.||'
type LOr :: Bool -> Bool -> Bool
type LOr x y = x || y

type LOrSym :: Bool ~> Bool ~> Bool
data LOrSym x
type instance App LOrSym x = LOrSym1 x

type LOrSym1 :: Bool -> Bool ~> Bool
data LOrSym1 x y
type instance App (LOrSym1 x) y = LOr x y

-------------------------------------------------------------------------------
-- Not
-------------------------------------------------------------------------------

-- | Logical not.
type NotSym :: Bool ~> Bool
data NotSym x
type instance App NotSym x = Not x
