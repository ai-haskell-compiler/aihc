{-# LANGUAGE DeriveDataTypeable #-}

module Impossible where

import Control.Exception
import Data.Typeable ( Typeable )


data Impossible
  = Impossible  String Integer
    -- ^ We reached a program point which should be unreachable.

  deriving Typeable

instance Show Impossible where
  show (Impossible file line) = unlines
    [ "An internal error has occurred. Please report this as a bug."
    , "Location of the error: " ++ file ++ ":" ++ show line
    ]

instance Exception Impossible

-- | Abort by throwing an \"impossible\" error. You should not use
-- this function directly. Instead use the macro in @undefined.h@.

throwImpossible :: Impossible -> a
throwImpossible = throw
