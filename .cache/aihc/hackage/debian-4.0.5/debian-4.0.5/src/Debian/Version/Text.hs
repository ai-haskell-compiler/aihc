{-# OPTIONS -fno-warn-orphans #-}
module Debian.Version.Text
    ( ParseDebianVersion(..)
    ) where

import Text.ParserCombinators.Parsec

import qualified Data.Text as T

import Debian.Version.Common
import Debian.Version.Internal

instance ParseDebianVersion T.Text where
    parseDebianVersion text =
        let str = T.unpack text in
        case parse parseDV str str of
          Left e -> Left e
          Right dv -> Right (DebianVersion str dv)
