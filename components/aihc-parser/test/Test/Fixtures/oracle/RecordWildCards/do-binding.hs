{- ORACLE_TEST pass -}
{-# LANGUAGE RecordWildCards #-}
module DoBinding where

x = do
  Loc {..} <- location
  pure ()
