{- ORACLE_TEST xfail record wildcard in do binding -}
{-# LANGUAGE RecordWildCards #-}
module DoBinding where

x = do
  Loc {..} <- location
  pure ()
