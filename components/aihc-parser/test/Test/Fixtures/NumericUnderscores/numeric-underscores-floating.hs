{- ORACLE_TEST
id: numeric-underscores-floating
category: literals
expected: pass
reason: parser now accepts NumericUnderscores floating literals
-}
{-# LANGUAGE NumericUnderscores #-}

module NumericUnderscoresFloating where

piApprox :: Double
piApprox = 3.141_592_653_589_793

avogadro :: Double
avogadro = 6.022_140_76e23
