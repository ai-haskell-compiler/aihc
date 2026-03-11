{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats
import Data.Proxy

main :: IO ()
main = print $ foo Proxy

foo :: (m + 1) ~ (n + 1) => Proxy m -> Proxy n
foo = id
