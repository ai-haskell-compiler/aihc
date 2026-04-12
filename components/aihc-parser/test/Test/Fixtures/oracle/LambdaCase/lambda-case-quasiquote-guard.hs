{- ORACLE_TEST pass -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module LambdaCaseQuasiQuoteGuard where

import Language.Haskell.TH.Quote (QuasiQuoter (..))

j :: QuasiQuoter
j =
  QuasiQuoter
    { quoteExp = \_ -> [| True |],
      quotePat = error "quotePat unused",
      quoteType = error "quoteType unused",
      quoteDec = error "quoteDec unused"
    }

a = \case
  x | [j|ok|] -> '5'
