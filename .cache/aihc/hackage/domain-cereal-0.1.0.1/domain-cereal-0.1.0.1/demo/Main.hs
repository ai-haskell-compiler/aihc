{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import Data.Serialize.Text ()
import Domain
import DomainCereal
import Prelude

declare
  Nothing
  (serializeDeriver <> genericDeriver <> eqDeriver)
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

main :: IO ()
main = do
  let value =
        NetworkServiceAddress
          $ NetworkAddress
            TcpTransportProtocol
            (IpHost (V4Ip 123))
            456
  let encoded = Cereal.encode value
  putStrLn $ "Encoded size: " <> show (ByteString.length encoded)
  case Cereal.decode encoded of
    Left err -> fail err
    Right res ->
      if value == res
        then putStrLn "Success! Decoded value does equal the original"
        else fail "Failure! Decoded valued doesn't equal the original"
