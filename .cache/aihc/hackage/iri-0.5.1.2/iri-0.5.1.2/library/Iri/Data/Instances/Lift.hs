{-# OPTIONS_GHC -Wno-orphans #-}

module Iri.Data.Instances.Lift where

import qualified Data.WideWord.Word128
import Iri.Data.Types
import Iri.Prelude
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax
import qualified Net.IPv6

fmap concat
  $ traverse deriveLift
  $ [ ''RegName,
      ''DomainLabel
    ]

instance Lift Host where
  lift = \case
    NamedHost regName ->
      AppE (ConE 'NamedHost) <$> lift regName
    IpV4Host ipV4 ->
      AppE (ConE 'IpV4Host) <$> $(makeLift ''IPv4) ipV4
    IpV6Host (Net.IPv6.IPv6 word128) -> do
      word128Exp <- $(makeLift ''Data.WideWord.Word128.Word128) word128
      pure (AppE (ConE 'IpV6Host) (AppE (ConE 'Net.IPv6.IPv6) word128Exp))

  liftTyped =
    Code . fmap TExp . lift

fmap concat
  $ traverse deriveLift
  $ [ ''Authority,
      ''Fragment,
      ''Hierarchy,
      ''HttpIri,
      ''Iri,
      ''Password,
      ''Path,
      ''PathSegment,
      ''Port,
      ''Query,
      ''Scheme,
      ''Security,
      ''User,
      ''UserInfo
    ]
