{-# LANGUAGE CPP             #-}

#if !defined(mingw32_HOST_OS)
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

{-|
Module:      Instances.System.Console.Terminfo
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @terminfo@ library.
-}
module Instances.System.Console.Terminfo () where

#if !defined(mingw32_HOST_OS)
import qualified Generics.Deriving.TH as Generics (deriveAll0)
import           Instances.Utils.GenericArbitrary (genericArbitrary)
import           System.Console.Terminfo (Color, SetupTermError)
import           Test.QuickCheck (Arbitrary(..))

$(Generics.deriveAll0 ''Color)
$(Generics.deriveAll0 ''SetupTermError)

instance Arbitrary Color where
    arbitrary = genericArbitrary

instance Arbitrary SetupTermError where
    arbitrary = genericArbitrary
#endif
