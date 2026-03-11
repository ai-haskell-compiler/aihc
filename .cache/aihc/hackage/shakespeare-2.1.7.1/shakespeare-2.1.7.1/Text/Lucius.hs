{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Lucius
    ( -- * Parsing
      lucius
    , luciusFile
    , luciusFileDebug
    , luciusFileReload
      -- ** Mixins
    , luciusMixin
    , Mixin
      -- ** Runtime
    , luciusRT
    , luciusRT'
    , luciusRTMinified
      -- *** Mixin
    , luciusRTMixin
    , RTValue (..)
    , -- * Datatypes
      Css
    , CssUrl
      -- * Type class
    , ToCss (..)
      -- * Rendering
    , renderCss
    , renderCssUrl
      -- * ToCss instances
      -- ** Color
    , Color (..)
    , colorRed
    , colorBlack
      -- ** Size
    , mkSize
    , AbsoluteUnit (..)
    , AbsoluteSize (..)
    , absoluteSize
    , EmSize (..)
    , ExSize (..)
    , PercentageSize (..)
    , percentageSize
    , PixelSize (..)
      -- * Internal
    , parseTopLevels
    , luciusUsedIdentifiers
    ) where

import Text.Internal.CssCommon
import Text.Internal.Lucius
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Text.Internal.Css

-- |
--
-- >>> renderCss ([lucius|foo{bar:baz}|] undefined)
-- "foo{bar:baz}"
lucius :: QuasiQuoter
lucius = luciusWithOrder Unordered

luciusFile :: FilePath -> Q Exp
luciusFile = luciusFileWithOrd Unordered

luciusFileDebug :: FilePath -> Q Exp
luciusFileDebug = luciusFileDebugWithOrder Unordered

luciusFileReload :: FilePath -> Q Exp
luciusFileReload = luciusFileDebug


luciusRT' :: TL.Text
          -> Either String ([(Text, Text)] -> Either String [TopLevel 'Resolved])
luciusRT' = luciusRTWithOrder' Unordered

luciusRT :: TL.Text -> [(Text, Text)] -> Either String TL.Text
luciusRT = luciusRTWithOrder Unordered


-- | Runtime Lucius with mixin support.
--
-- Since 1.0.6
luciusRTMixin :: TL.Text -- ^ template
              -> Bool -- ^ minify?
              -> [(Text, RTValue)] -- ^ scope
              -> Either String TL.Text
luciusRTMixin = luciusRTMixinWithOrder Unordered

-- | Same as 'luciusRT', but output has no added whitespace.
--
-- Since 1.0.3
luciusRTMinified :: TL.Text -> [(Text, Text)] -> Either String TL.Text
luciusRTMinified = luciusRTMinifiedWithOrder Unordered

luciusMixin :: QuasiQuoter
luciusMixin = luciusMixinWithOrder Unordered

