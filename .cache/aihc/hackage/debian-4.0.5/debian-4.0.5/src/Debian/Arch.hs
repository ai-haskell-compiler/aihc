{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Debian.Arch
    ( Arch(..)
    , ArchOS(..)
    , ArchCPU(..)
    , prettyArch
    , parseArch
    ) where

import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Text.PrettyPrint (Doc, text)

data ArchOS = ArchOS String | ArchOSAny deriving (Eq, Ord, Read, Show, Data, Typeable)

prettyOS :: ArchOS -> Doc
prettyOS (ArchOS s) = text s
prettyOS ArchOSAny = text "any"

parseOS :: String -> ArchOS
parseOS "any" = ArchOSAny
parseOS s = ArchOS s

data ArchCPU = ArchCPU String | ArchCPUAny deriving (Eq, Ord, Read, Show, Data, Typeable)

prettyCPU :: ArchCPU -> Doc
prettyCPU (ArchCPU s) = text s
prettyCPU ArchCPUAny = text "any"

parseCPU :: String -> ArchCPU
parseCPU "any" = ArchCPUAny
parseCPU s = ArchCPU s

data Arch
    = Source
    | All
    | Binary ArchOS ArchCPU
    deriving (Eq, Ord, Read, Show, Data, Typeable)

prettyArch :: Arch -> Doc
prettyArch Source = text "source"
prettyArch All = text "all"
prettyArch (Binary (ArchOS "linux") cpu) = prettyCPU cpu
prettyArch (Binary os cpu) = prettyOS os <> text "-" <> prettyCPU cpu

parseArch :: String -> Arch
parseArch s =
    case span (/= '-') s of
      ("source", "") -> Source
      ("all", "") -> All
      (cpu, "") -> Binary (ArchOS "linux") (parseCPU cpu)
      (os, '-' : cpu) -> Binary (parseOS os) (parseCPU cpu)
      _ -> error "parseArch: internal error"
