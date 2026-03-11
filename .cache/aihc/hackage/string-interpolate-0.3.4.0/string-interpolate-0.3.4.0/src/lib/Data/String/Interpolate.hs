-- |
-- Module      : Data.String.Interpolate
-- Description : Unicode-aware string interpolation that handles all textual types.
-- Copyright   : (c) William Yao, 2019-2023
-- License     : BSD-3
-- Maintainer  : williamyaoh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides three quasiquoters, `i', `__i', and `iii', which:
--
-- * handle all of String\/Text\/ByteString, both strict and lazy
-- * can interpolate /into/ anything that implements `IsString'
-- * can interpolate anything that implements `Show'
-- * are Unicode aware
-- * are fast
-- * handle multiline strings
--
-- `i' leaves newlines and whitespace intact as they are in the source
-- code. `__i' strips leading indentation and surrounding blank lines, while
-- leaving linebreaks intact. `iii' collapses newlines/whitespace into single
-- spaces, putting all the output on a single line.
--
-- As an example,
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE QuasiQuotes #-}
-- >
-- > import Data.Text
-- > import Data.String.Interpolate ( i )
-- >
-- > λ> age = 33 :: Int
-- > λ> name = "Tatiana" :: Text
-- > λ> [i|{"name": "#{name}", "age": #{age}}|] :: String
-- > >>> "{\"name\": \"Tatiana\", \"age\": 33}"
-- >
-- > λ> [i|
-- > Name: #{name}
-- > Age: #{age}
-- > |] :: String
-- > >>> "\nName: Tatiana\nAge: 33\n"
--
-- There are also variants of `__i' and `iii' which have different behavior
-- for surrounding newlines.
--
-- See the README at <https://gitlab.com/williamyaoh/string-interpolate/blob/master/README.md>
-- for more details and examples.

{-# LANGUAGE TemplateHaskell #-}

module Data.String.Interpolate
  (
    -- * Basic interpolators
    i, __i, iii
    -- * Interpolator variants for newline handling
  , __i'E, __i'L, iii'E, iii'L
  )
where

import Control.Monad ( (<=<) )

import Data.Foldable ( traverse_ )
import Data.List     ( intercalate )

import qualified Language.Haskell.Exts.Extension as Ext
import           Language.Haskell.Exts.Parser
  ( ParseMode(..), ParseResult(..), defaultParseMode, parseExpWithMode )
import           Language.Haskell.Meta           ( ToExp(..) )
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote       ( QuasiQuoter(..) )

import Data.String.Interpolate.Conversion ( proxyWrapper, build, finalize, interpolate, ofString )

import Data.String.Interpolate.Lines      ( IndentWarning(..), Mindent(..), handleIndents )
import Data.String.Interpolate.Parse
import Data.String.Interpolate.Types
import Data.String.Interpolate.Whitespace ( collapseWhitespace )

data OutputSegment
  = OfString String
  | Interpolate String

-- |
-- Singleton list of the first element, if there is one.
fore :: [a] -> [a]
fore []    = []
fore (x:_) = [x]

-- |
-- Singleton list of the last element, if there is one.
aft :: [a] -> [a]
aft []     = []
aft [x]    = [x]
aft (_:xs) = aft xs

collapseStrings :: [OutputSegment] -> [OutputSegment]
collapseStrings [] = []
collapseStrings (OfString s1 : OfString s2 : rest) =
  collapseStrings ((OfString $ s1 ++ s2) : rest)
collapseStrings (other : rest) = other : collapseStrings rest

renderLines :: Lines -> [OutputSegment]
renderLines = intercalate [OfString "\n"] . fmap renderLine
  where
    renderLine :: Line -> [OutputSegment]
    renderLine = fmap renderSegment

    renderSegment :: InterpSegment -> OutputSegment
    renderSegment (Expression expr) = Interpolate expr
    renderSegment (Verbatim str)    = OfString str
    renderSegment (Spaces n)        = OfString (replicate n ' ')
    renderSegment (Tabs n)          = OfString (replicate n '\t')

-- |
-- Produce the final Template Haskell expression. Handles collapsing
-- intermediate strings.
outputToExp :: [OutputSegment] -> Q Exp
outputToExp segs = finalExp
  where
    finalExp = [| proxyWrapper $ \proxy -> $(mkFinalize [| proxy |]) |]

    mkFinalize :: Q Exp -> Q Exp
    mkFinalize proxy = [|finalize $proxy $(go proxy (collapseStrings segs))|]

    go :: Q Exp -> [OutputSegment] -> Q Exp
    go proxy = foldr
      (\seg qexp -> [|build $proxy $(renderExp proxy seg) $(qexp)|])
      [|ofString $proxy ""|]

    renderExp :: Q Exp -> OutputSegment -> Q Exp
    renderExp proxy (OfString str)     = [|ofString $proxy str|]
    renderExp proxy (Interpolate expr) = [|interpolate $proxy $(reifyExpression expr)|]

type Interpolator = ParseOutput -> Q Lines

-- |
-- Fundamentally all our interpolators are, are functions from the parse
-- input to some transformed lines. The rest is just boilerplate.
interpolator :: String -> Interpolator -> QuasiQuoter
interpolator qqName transform = QuasiQuoter
  { quoteExp  =
      outputToExp
        <=< (pure . renderLines)
        <=< transform
        <=< unwrap qqName . parseInput . dosToUnix
  , quotePat  = const $ errQQType qqName "pattern"
  , quoteType = const $ errQQType qqName "type"
  , quoteDec  = const $ errQQType qqName "declaration"
  }

-- |
-- The basic, no-frills interpolator. Will interpolate anything you wrap in @#{}@, and
-- otherwise leaves what you write alone.
i :: QuasiQuoter
i = interpolator "i" transform
  where
    transform :: Interpolator
    transform (ParseOutput header content footer) =
      pure $! mconcat [header, content, footer]

-- |
-- An interpolator that handles indentation. Will interpolate anything you wrap in @#{}@,
-- remove leading indentation, and remove any blank lines before and after the content.
--
-- If the contained interpolation uses both tabs and spaces for indentation, @__i@
-- will assume the indentation type it finds in the first nonblank line, ignoring
-- indentation of the other type. Please don't use mixed indentation.
--
-- Note that only indentation you actually write in source code will be stripped;
-- @__i@ does not touch any lines or whitespace inserted by interpolations themselves.
--
-- There is no extra performance penalty for using @__i@.
__i :: QuasiQuoter
__i = interpolator "__i" transform
  where
    transform :: Interpolator
    transform (ParseOutput _ content _) = do
      let (warns, withoutIndent) = handleIndents content
      traverse_ reportIndentWarning warns
      pure $! withoutIndent

-- |
-- Like `__i', but leaves any surrounding newlines intact.
--
-- The way to remember which is which is to look at the suffix character;
-- the multiple horizontal lines of the capital @E@ suggests multiple
-- textual lines.
__i'E :: QuasiQuoter
__i'E = interpolator "__i'E" transform
  where
    transform :: Interpolator
    transform (ParseOutput header content footer) = do
      let (warns, withoutIndent) = handleIndents content
      traverse_ reportIndentWarning warns
      pure $! mconcat [header, withoutIndent, footer]

-- |
-- Like `__i', but collapses any surrounding newlines into a single newline.
--
-- The way to remember which is which is to look at the suffix character;
-- the single horizontal line of the capital @L@ suggests that it leaves
-- only a single newline.
__i'L :: QuasiQuoter
__i'L = interpolator "__i'L" transform
  where
    transform :: Interpolator
    transform (ParseOutput header content footer) = do
      let (warns, withoutIndent) = handleIndents content
      traverse_ reportIndentWarning warns
      pure $! mconcat [aft header, withoutIndent, fore footer]

-- |
-- An interpolator that strips excess whitespace. Will collapse any sequences of
-- multiple spaces or whitespace into a single space, putting the output onto a
-- single line with surrounding whitespace removed.
--
-- Note that only whitespace you actually write in source code will be collapsed;
-- @iii@ does not touch any lines or whitespace inserted by interpolations themselves.
--
-- There is no extra performance penalty for using @iii@.
iii :: QuasiQuoter
iii = interpolator "iii" transform
  where
    transform :: Interpolator
    transform (ParseOutput _ content _) =
      pure $! [collapseWhitespace content]

-- |
-- Like `iii', but leaves any surrounding newlines intact.
--
-- The way to remember which is which is to look at the suffix character;
-- the multiple horizontal lines of the capital @E@ suggests multiple
-- textual lines.
iii'E :: QuasiQuoter
iii'E = interpolator "iii'E" transform
  where
    transform :: Interpolator
    transform (ParseOutput header content footer) =
      let collapsed = collapseWhitespace content
      in pure $! mconcat [header, [collapsed], footer]

-- |
-- Like `iii', but collapses any surrounding newlines into a single newline.
--
-- The way to remember which is which is to look at the suffix character;
-- the single horizontal line of the capital @L@ suggests that it leaves
-- only a single newline.
iii'L :: QuasiQuoter
iii'L = interpolator "iii'L" transform
  where
    transform :: Interpolator
    transform (ParseOutput header content footer) =
      let collapsed = collapseWhitespace content
      in pure $! mconcat [aft header, [collapsed], fore footer]

--------------------
-- UTILITIES
--------------------

errQQ :: String -> String -> Q a
errQQ qqName msg =
  fail ("Data.String.Interpolate." ++ qqName ++ ": " ++ msg)

errQQType :: String -> String -> Q a
errQQType qqName = errQQ qqName . ("This QuasiQuoter cannot be used as a " ++)

unwrap :: String -> Either String a -> Q a
unwrap = unwrapWith id

unwrapWith :: (err -> String) -> String -> Either err a -> Q a
unwrapWith f qqName e = case e of
  Left err -> errQQ qqName $ f err
  Right x  -> pure x

reifyExpression :: String -> Q Exp
reifyExpression s = do
  -- We want to explicitly use whatever extensions are enabled in current module
  exts      <- (fmap . fmap) (Ext.parseExtension . show) extsEnabled
  parseMode <- pure (defaultParseMode { extensions = exts })
  case parseExpWithMode parseMode s of
    ParseFailed _ err  -> fail $
      "Data.String.Interpolate.i: got error: '" ++ err ++ "' while parsing expression: " ++ s
    ParseOk e -> pure (toExp e)

reportIndentWarning :: IndentWarning -> Q ()
reportIndentWarning (IndentWarning line base) = do
  let
    header = case base of
      UsesSpaces _ -> "found TAB in SPACE-based indentation on this line:"
      UsesTabs _   -> "found SPACE in TAB-based indentation on this line:"
    message =
         header <> "\n\n"
      <> "  " <> line <> "\n"
  reportWarning message
