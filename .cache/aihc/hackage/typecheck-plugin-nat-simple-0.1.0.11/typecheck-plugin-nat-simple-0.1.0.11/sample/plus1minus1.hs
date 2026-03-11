{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats
import Data.Proxy

main :: IO ()
main = print $ foo Proxy

foo :: Proxy (n + 1 - 1) -> Proxy n
foo = id
