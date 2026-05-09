{- ORACLE_TEST pass -}
module ForceLayoutParenthesizedLensArg where

stepPos p = pos %~ (.+^ p ^. vel) $ p
