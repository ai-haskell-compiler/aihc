{-# LANGUAGE CPP #-}
{-|
Module:      Spec.System.Console.TerminfoSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @terminfo@ library.
-}
module Spec.System.Console.TerminfoSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if !defined(mingw32_HOST_OS)
import Data.Proxy (Proxy(..))
import Instances.System.Console.Terminfo ()
import Spec.Utils (matchesTextShowSpec)
import System.Console.Terminfo (Color, SetupTermError)
import Test.Hspec (describe)
import TextShow.System.Console.Terminfo ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if !defined(mingw32_HOST_OS)
    describe "Color" $
        matchesTextShowSpec (Proxy :: Proxy Color)
    describe "SetupTermError" $
        matchesTextShowSpec (Proxy :: Proxy SetupTermError)
#else
    pure ()
#endif
