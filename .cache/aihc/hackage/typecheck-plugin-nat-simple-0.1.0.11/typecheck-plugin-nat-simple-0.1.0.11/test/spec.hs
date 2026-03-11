{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Derivation.CanDerive
import Data.Derivation.Parse

import Control.Monad.Try

main :: IO ()
main = do
	print $ wanted @String @String =<< maybeToTry "parse error" (parseConstraint "0 == n - n")
