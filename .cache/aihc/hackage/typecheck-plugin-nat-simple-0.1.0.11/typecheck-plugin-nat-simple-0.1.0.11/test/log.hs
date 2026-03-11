{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

import GHC.TypeNats
import Data.Proxy

foo :: 1 <= n => Proxy (n - 1 + 1) -> Proxy (n + 1 - 1)
foo = id

bar :: 1 <= a => Proxy (a + 1 - 1) -> Proxy (a - 1 + 1)
bar = id

baz :: 1 <= b => Proxy (a + b) -> Proxy ((a + 1) + (b - 1))
baz = id

hoge :: Proxy (a - 1) -> Proxy (a - 1)
hoge = id

main :: IO ()
main = putStrLn "test log"
