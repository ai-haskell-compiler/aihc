{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Word
import Data.Int
import Data.DoubleWord.TH

mkUnpackedDoubleWord "U64" ''Word32 "I64" ''Int32 ''Word32 []
mkUnpackedDoubleWord "U48" ''Word16 "I48" ''Int16 ''Word32 []
mkUnpackedDoubleWord "UU64" ''Word16 "II64" ''Int16 ''U48 []

