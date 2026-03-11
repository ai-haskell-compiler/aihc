{-|
Module      : Language.Alloy.Call
Copyright   : (c) Marcellus Siegburg, 2019 - 2021
License     : MIT

This module provides functions to retrieve raw instances form Alloy,
i.e. as Alloy provides them.
-}
module Language.Alloy.Debug (
  getRawInstances,
  getRawInstancesWith,
  parseInstance,
  ) where

import Language.Alloy.Internal.Call     (getRawInstances, getRawInstancesWith)
import Language.Alloy.Parser            (parseInstance)
