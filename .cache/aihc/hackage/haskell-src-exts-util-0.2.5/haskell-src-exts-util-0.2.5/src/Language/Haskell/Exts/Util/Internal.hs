{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | The contents of this module originate from module
--  [HSE.Util](https://github.com/ndmitchell/hlint/blob/master/src/HSE/Util.hs)
--  in Neil Mitchell's HLint.
module Language.Haskell.Exts.Util.Internal where

import           Language.Haskell.Exts
import           Prelude

---------------------------------------------------------------------
-- ACCESSOR/TESTER

-- is* :: Exp l -> Bool
-- is* :: Decl s -> Bool
isApp :: Exp l -> Bool
isApp App{} = True; isApp _ = False
isAnyApp :: Exp l -> Bool
isAnyApp x = isApp x || isInfixApp x
isInfixApp :: Exp l -> Bool
isInfixApp InfixApp{} = True; isInfixApp _ = False

isDot :: QOp s -> Bool
isDot (QVarOp _ (UnQual _ (Symbol _ "."))) = True
isDot _                                    = False
isSection :: Exp l -> Bool
isSection LeftSection{}  = True
isSection RightSection{} = True
isSection _              = False

isDotApp :: Exp s -> Bool
isDotApp (InfixApp _ _ dot _) | isDot dot = True
isDotApp _                    = False
