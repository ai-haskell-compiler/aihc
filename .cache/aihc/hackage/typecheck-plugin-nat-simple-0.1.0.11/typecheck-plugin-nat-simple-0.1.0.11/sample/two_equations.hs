{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats
import Data.Proxy

main :: IO ()
main = print $ foo Proxy

foo :: ((x + y) ~ z, (w - y) ~ v) => Proxy (x + w) -> Proxy (z + v)
foo = id
