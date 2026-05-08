{- ORACLE_TEST xfail Guard roundtrip due to extra parentheses around operator argument -}
module ForceLayoutParenthesizedLensArg where

stepPos p = pos %~ (.+^ p ^. vel) $ p

