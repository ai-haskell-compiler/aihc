{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Curry where

import GHC.Exts
import Data.Curry.TH

concat <$> crr `mapM` [0 .. maxTupleSize]
concat <$> unc `mapM` [0 .. maxTupleSize]
