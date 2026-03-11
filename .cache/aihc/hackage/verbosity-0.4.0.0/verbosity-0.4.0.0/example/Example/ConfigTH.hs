{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:       $HEADER$
-- Description:  Example of HasVerbosity instance defined using
--               TemplateHaskell code from lens package.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Example of HasVerbosity instance defined using TemplateHaskell code from
-- <https://hackage.haskell.org/package/lens lens> package.
module Example.ConfigTH
    ( Config
    , module Data.Verbosity.Class
    )
  where

import Control.Lens.TH (makeLenses)

import Data.Verbosity.Class


data Config = Config
    { _appVerbosity :: Verbosity
--  , ...
    }
  deriving Show

makeLenses ''Config

instance HasVerbosity Config where
    verbosity = appVerbosity
