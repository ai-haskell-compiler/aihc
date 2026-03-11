{-# LANGUAGE TemplateHaskell #-}

module Data.MakeEnum.Options(
    OptionsT(..),
    Options,
    defaultOptions
  ) where

import Language.Haskell.TH

data OptionsT f = Options
  { newEnumName :: f String
  , fromFunctionName :: f String
  , toFunctionName :: f String
  , ctorNameModifier :: String -> String
  , deriveClasses :: [Name]
  }

type Options = OptionsT Maybe

defaultOptions :: Options
defaultOptions = Options Nothing Nothing Nothing id [''Eq, ''Ord, ''Show]
