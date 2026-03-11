module Main where

import Domain
import DomainAeson
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Classes
import Test.QuickCheck.Instances ()
import Prelude

declare
  Nothing
  (eqDeriver <> showDeriver <> genericDeriver <> toJsonDeriver <> fromJsonDeriver)
  [schema|

    ServiceAddress:
      sum:
        network: NetworkAddress
        local: FilePath

    NetworkAddress:
      product:
        protocol: TransportProtocol
        host: Host
        port: Word16

    TransportProtocol:
      enum:
        - tcp
        - udp

    Host:
      sum:
        ip: Ip
        name: Text

    Ip:
      sum:
        v4: Word32
        v6: Word128

    Word128:
      product:
        part1: Word64
        part2: Word64

    |]

deriving via (GenericArbitrary ServiceAddress) instance Arbitrary ServiceAddress

deriving via (GenericArbitrary NetworkAddress) instance Arbitrary NetworkAddress

deriving via (GenericArbitrary TransportProtocol) instance Arbitrary TransportProtocol

deriving via (GenericArbitrary Host) instance Arbitrary Host

deriving via (GenericArbitrary Ip) instance Arbitrary Ip

deriving via (GenericArbitrary Word128) instance Arbitrary Word128

main :: IO ()
main =
  hspec $ do
    lawsSpec $ jsonLaws (Proxy @ServiceAddress)
    lawsSpec $ jsonLaws (Proxy @NetworkAddress)
    lawsSpec $ jsonLaws (Proxy @TransportProtocol)
    lawsSpec $ jsonLaws (Proxy @Host)
    lawsSpec $ jsonLaws (Proxy @Ip)
    lawsSpec $ jsonLaws (Proxy @Word128)

lawsSpec :: Laws -> Spec
lawsSpec (Laws name properties) =
  describe name $ for_ properties $ \(lawName, property) ->
    prop lawName property
