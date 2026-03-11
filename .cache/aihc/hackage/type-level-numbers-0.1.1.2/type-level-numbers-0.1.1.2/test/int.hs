{-# LANGUAGE TemplateHaskell #-}
import Control.Applicative
import Language.Haskell.TH
import System.Exit

import TestNat


main :: IO ()
main = do
  plus  <- sequence $(listE (testAddZ <$> [-9..9] <*> [-9..9]))
  minus <- sequence $(listE (testSubZ <$> [-9..9] <*> [-9..9]))
  mult  <- sequence $(listE (testMulZ <$> [-9..9] <*> [-9..9]))
  case and $ plus ++ minus ++ mult of
    True  -> exitSuccess
    False -> exitFailure
