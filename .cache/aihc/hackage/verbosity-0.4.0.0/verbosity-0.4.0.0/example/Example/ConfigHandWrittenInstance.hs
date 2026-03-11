-- |
-- Module:       $HEADER$
-- Description:  Example of hand written HasVerbosity instance.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Example of hand written HasVerbosity instance.
module Example.Config
    ( Config
    , module Data.Verbosity.Class
    )
  where

import Data.Functor ((<$>))

import Data.Verbosity.Class


data Config = Config
    { _appVerbosity :: Verbosity
--  , ...
    }
  deriving Show

instance HasVerbosity Config where
    verbosity f c@Config{_appVerbosity = a} =
        (\b -> c{_appVerbosity = b}) <$> f a
