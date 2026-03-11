{-# LANGUAGE CPP #-}

module OpenSuse.Prelude
  (
    -- * Standard Prelude
    module Prelude
  , module Control.Monad.Extra
  , module Control.Monad.Fail
  , module Control.Monad.IO.Class
  , module Data.Monoid
  , module Data.Semigroup
  , module Data.Word
  , module GHC.Generics
  , module Numeric.Natural

    -- * Parsing & Pretty Printing
  , Text, LazyText, ByteString, LazyByteString
  , packText, unpackText
  , module OpenSuse.Prelude.Parser
  , module OpenSuse.Prelude.PrettyPrinting
  , module Data.Aeson
  , module Data.String

    -- * Date & Time
  , module Data.Time.Clock

    -- * Container
  , module Data.Set

    -- * Miscellaneous
  , module Control.DeepSeq
  , module Data.Binary
  , module Data.Hashable
  , module Data.Maybe

  )
  where

import Prelude hiding ( fail
#if MIN_VERSION_base(4,11,0)
                      , (<>)  -- https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid
#endif
                      )

import OpenSuse.Prelude.Parser ( CharParser, HasParser(..), ErrorContext, parseM, parse
                               , runParser, runParserT
                               )
import OpenSuse.Prelude.PrettyPrinting ( Doc, Pretty(pPrint), prettyShow )

import Control.DeepSeq ( NFData )
import Control.Monad.Extra hiding ( fail )
import Control.Monad.Fail
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Aeson ( FromJSON, ToJSON )
import Data.Binary ( Binary )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as LBS
import Data.Hashable ( Hashable )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( Monoid(..) )
import Data.Semigroup ( Semigroup(..) )
import Data.Set ( Set )
import Data.String ( IsString(..) )
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Time.Clock ( UTCTime(..), DiffTime )
import Data.Word ( Word8 )
import GHC.Generics ( Generic )
import Numeric.Natural ( Natural )

type LazyText = LText.Text
type LazyByteString = LBS.ByteString

packText :: String -> Text
packText = Text.pack

unpackText :: Text -> String
unpackText = Text.unpack
