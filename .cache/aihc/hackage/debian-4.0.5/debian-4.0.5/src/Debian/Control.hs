{-# LANGUAGE OverloadedStrings #-}
-- |A module for working with Debian control files <http://www.debian.org/doc/debian-policy/ch-controlfields.html>
module Debian.Control
    ( -- * Types
      Control'(..)
    , Paragraph'(..)
    , Field'(..)
    , Control
    , Paragraph
    , Field
    , ControlParser
    , ControlFunctions(..)
    -- * Control File Parser
    , pControl
    -- * Helper Functions
    , mergeControls
    , fieldValue
    , removeField
    , prependFields
    , appendFields
    , renameField
    , modifyField
    , raiseFields
    , packParagraph
    , packField
    , formatControl
    , formatParagraph
    , formatField
    -- * Policy classes and functions
    , P.HasDebianControl(..)
    , P.ControlFileError(..)
    , P.parseDebianControlFromFile
    , P.validateDebianControl
    , P.unsafeDebianControl
    , P.debianSourceParagraph
    , P.debianBinaryParagraphs
    , P.debianPackageParagraphs
    , P.debianPackageNames
    , P.debianSourcePackageName
    , P.debianBinaryPackageNames
    , P.debianRelations
    , P.debianBuildDeps
    , P.debianBuildDepsIndep
    ) where

--import Control.Monad
--import Data.List
--import Text.ParserCombinators.Parsec
--import System.IO
import Debian.Control.Common
import Debian.Control.String
import Data.List
import Data.Text as T (Text, pack, concat)
import qualified Debian.Control.Builder ()
import qualified Debian.Control.Text as T
--import qualified Debian.Control.TextLazy as TL
import qualified Debian.Control.ByteString as B ()
import qualified Debian.Control.Policy as P
import qualified Debian.Control.String as S

packParagraph :: S.Paragraph -> T.Paragraph
packParagraph (S.Paragraph s) = T.Paragraph (map packField s)

packField :: Field' String -> Field' Text
packField (S.Field (name, value)) = T.Field (T.pack name, T.pack value)
packField (S.Comment s) = T.Comment (T.pack s)

formatControl :: Control' Text -> [Text]
formatControl (T.Control paragraphs) = intersperse (T.pack "\n") . map formatParagraph $ paragraphs

formatParagraph :: Paragraph' Text -> Text
formatParagraph (T.Paragraph fields) = T.concat . map formatField $ fields

formatField :: Field' Text -> Text
formatField (T.Field (name, value)) = T.concat [name, T.pack ":", value, T.pack "\n"]
formatField (T.Comment s) = s
