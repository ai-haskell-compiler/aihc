{-# OPTIONS -fno-warn-orphans #-}
module Language.Haskell.Exts.Util
  ( -- * Free variables of ASTs
    FreeVars(..)
  , Vars(..)
  , AllVars(..)
    -- * Rebracketing of ASTs
  , Brackets(..)
  , paren
  , transformBracket
  , rebracket1
  , appsBracket

  ) where

import Data.Default
import Language.Haskell.Exts hiding (paren)
import Language.Haskell.Exts.Bracket
import Language.Haskell.Exts.FreeVars

-- Orphan instances required for using the bracketing code with HSE source locs
instance Default SrcLoc where
  def = noLoc
  
instance Default SrcSpan where
  def = srcInfoSpan noSrcSpan

instance Default SrcSpanInfo where
  def = noSrcSpan
