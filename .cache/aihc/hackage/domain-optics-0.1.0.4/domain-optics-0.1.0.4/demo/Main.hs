{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Domain
import DomainOptics
import Prelude

main :: IO ()
main =
  return ()

declare
  Nothing
  labelOpticDeriver
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
