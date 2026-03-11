{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Debian.Version.String
    ( ParseDebianVersion(..)
    ) where

import Text.ParserCombinators.Parsec

import Data.List (stripPrefix)
import Debian.Version.Common
import Debian.Version.Internal

instance ParseDebianVersion String where
    parseDebianVersion str =
        case parse parseDV str str of
          Left e -> Left e
          Right dv -> Right (DebianVersion str dv)
 
instance Read DebianVersion where
    readsPrec _ s =
        case stripPrefix "Debian.Version.parseDebianVersion " s of
          Just s' -> case reads s' :: [(String, String)] of
                       []-> []
                       (v, s'') : _ -> [(parseDebianVersion' v, s'')]
          Nothing -> []
