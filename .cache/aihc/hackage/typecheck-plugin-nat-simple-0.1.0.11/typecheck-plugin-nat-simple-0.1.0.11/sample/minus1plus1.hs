{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats
import Data.Proxy

main :: IO ()
main = print $ foo @1 Proxy

-- foo :: Proxy (n - 1 + 1) -> Proxy n
foo :: 1 <= n => Proxy (n - 1 + 1) -> Proxy n
foo = id
