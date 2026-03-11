{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-orphans #-}
module TextShow.Data.Aeson where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

import Prelude ()
import Prelude.Compat

import TextShow ( TextShow(..), TextShow1(..)
                , fromText, showbParen, showbPrec1, showtToShowb, singleton )
import TextShow.Data.Scientific ()
import TextShow.Data.Vector ()
import TextShow.Utils (showbUnaryListWith)

instance TextShow K.Key where
    showb = showtToShowb showt
    showt = showt . K.toText

instance TextShow Value where
    showbPrec _ Null = fromText "Null"
    showbPrec d (Bool b) = showbParen (d > 10)
        $ fromText "Bool " <> showbPrec 11 b
    showbPrec d (Number s) = showbParen (d > 10)
        $ fromText "Number " <> showbPrec 11 s
    showbPrec d (String s) = showbParen (d > 10)
        $ fromText "String " <> showbPrec 11 s
    showbPrec d (Array xs) = showbParen (d > 10)
        $ fromText "Array " <> showbPrec 11 xs
    showbPrec d (Object xs) = showbParen (d > 10)
        $ fromText "Object (fromList "
        <> showbPrec 11 (KM.toAscList xs)
        <> singleton ')'

instance TextShow v => TextShow (KM.KeyMap v) where
    showbPrec = showbPrec1

instance TextShow1 KM.KeyMap where
    liftShowbPrec sp sl d xs =
      showbUnaryListWith (liftShowbList sp sl) d (KM.toAscList xs)
