{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Representations of a web page, covering Html, CSS & JS artifacts.
module Web.Rep.Page
  ( Page (..),
    PageConfig (..),
    defaultPageConfig,
    Concerns (..),
    suffixes,
    concernNames,
    PageConcerns (..),
    PageStructure (..),
    Css (..),
    renderCss,
    cssColorScheme,
    Js (..),
    onLoad,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import GHC.Generics
import MarkupParse
import Optics.Core

-- | Components of a web page.
--
-- A web page can take many forms but still have the same underlying representation. For example, CSS can be linked to in a separate file, or can be inline within html, but still be the same css and have the same expected external effect. A Page represents the practical components of what makes up a static snapshot of a web page.
data Page = Page
  { -- | css library links
    libsCss :: Markup,
    -- | javascript library links
    libsJs :: Markup,
    -- | css
    cssBody :: Css,
    -- | javascript with global scope
    jsGlobal :: Js,
    -- | javascript included within the onLoad function
    jsOnLoad :: Js,
    -- | html within the header
    htmlHeader :: Markup,
    -- | body html
    htmlBody :: Markup
  }
  deriving (Show, Generic)

instance Semigroup Page where
  (<>) p0 p1 =
    Page
      (p0 ^. #libsCss <> p1 ^. #libsCss)
      (p0 ^. #libsJs <> p1 ^. #libsJs)
      (p0 ^. #cssBody <> p1 ^. #cssBody)
      (p0 ^. #jsGlobal <> p1 ^. #jsGlobal)
      (p0 ^. #jsOnLoad <> p1 ^. #jsOnLoad)
      (p0 ^. #htmlHeader <> p1 ^. #htmlHeader)
      (p0 ^. #htmlBody <> p1 ^. #htmlBody)

instance Monoid Page where
  mempty = Page mempty mempty mempty mempty mempty mempty mempty

  mappend = (<>)

-- | A web page typically is composed of some css, javascript and html.
--
-- 'Concerns' abstracts this structural feature of a web page.
data Concerns a = Concerns
  { cssConcern :: a,
    jsConcern :: a,
    htmlConcern :: a
  }
  deriving (Eq, Show, Foldable, Traversable, Generic)

instance Functor Concerns where
  fmap f (Concerns c j h) = Concerns (f c) (f j) (f h)

instance Applicative Concerns where
  pure a = Concerns a a a

  Concerns f g h <*> Concerns a b c = Concerns (f a) (g b) (h c)

-- | The common file suffixes of the three concerns.
suffixes :: Concerns FilePath
suffixes = Concerns ".css" ".js" ".html"

-- | Create filenames for each Concern element.
concernNames :: FilePath -> FilePath -> Concerns FilePath
concernNames dir stem =
  (\x -> dir <> stem <> x) <$> suffixes

-- | Is the rendering to include all 'Concerns' (typically in a html file) or be separated (tyypically into separate files and linked in the html file)?
data PageConcerns
  = Inline
  | Separated
  deriving (Show, Eq, Generic)

-- | Various ways that a Html file can be structured.
data PageStructure
  = HeaderBody
  | Headless
  | Snippet
  deriving (Show, Eq, Generic)

-- | Configuration options when rendering a 'Page'.
data PageConfig = PageConfig
  { concerns :: PageConcerns,
    structure :: PageStructure,
    renderStyle :: RenderStyle,
    filenames :: Concerns FilePath,
    localdirs :: [FilePath]
  }
  deriving (Show, Eq, Generic)

-- | Default configuration is inline ecma and css, separate html header and body, minified code, with the suggested filename prefix.
defaultPageConfig :: FilePath -> PageConfig
defaultPageConfig stem =
  PageConfig
    Inline
    HeaderBody
    Compact
    ((stem <>) <$> suffixes)
    []

-- | css as a string.
newtype Css = Css {cssByteString :: ByteString} deriving (Show, Eq, Generic, Semigroup, Monoid)

-- | Render 'Css' as text.
renderCss :: RenderStyle -> Css -> ByteString
renderCss Compact = C.filter (\c -> c /= ' ' && c /= '\n') . cssByteString
renderCss _ = cssByteString

-- | Css snippet for reponsiveness to preferred color-scheme.
cssColorScheme :: Css
cssColorScheme =
  Css
    "{\n  color-scheme: light dark;\n}\n{\n  body {\n    background-color: rgb(92%, 92%, 92%);\n    color: rgb(5%, 5%, 5%);\n  }\n}\n@media (prefers-color-scheme:dark) {\n  body {\n    background-color: rgb(5%, 5%, 5%);\n    color: rgb(92%, 92%, 92%);\n  }\n}"

-- | Javascript as string
newtype Js = Js {jsByteString :: ByteString} deriving (Eq, Show, Generic, Semigroup, Monoid)

-- | Add the windows.onload assignment
onLoad :: Js -> Js
onLoad (Js t) = Js (" window.onload=function(){" <> t <> "};")
