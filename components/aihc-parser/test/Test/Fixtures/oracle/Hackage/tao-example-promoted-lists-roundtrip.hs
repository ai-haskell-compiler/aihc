{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module TaoExamplePromotedListsRoundtrip where

import Data.Proxy (Proxy(Proxy))

type OneToFour = '[1, 2, 3, 4]
type Empty = '[]

unitTests :: Proxy ('[ '[1, 2, 3], '[4] ])
unitTests = Proxy :: Proxy ('[ '[1, 2, 3], '[4] ])
