{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

import GHC.TypeNats
import Data.Proxy

main :: forall (a :: Nat) . IO ()
main = do
	print (foo (Proxy :: Proxy (a + 3)) :: Proxy (a + 3))
--	print (bar (Proxy :: Proxy (a + 3)) :: Proxy (a + 3))
--	print (bar (Proxy :: Proxy a) :: Proxy a)
	print (bar (Proxy :: Proxy 123) :: Proxy 123)
-- main = print (foo (Proxy :: Proxy a) :: Proxy a)

foo :: ((m + n) ~ (j + k), (n - j) ~ 3) => Proxy (m + 3) -> Proxy k
foo = id

bar :: ((m + n) ~ (j + k), (n - j) ~ 3) => Proxy k -> Proxy (m + 3)
bar = id
