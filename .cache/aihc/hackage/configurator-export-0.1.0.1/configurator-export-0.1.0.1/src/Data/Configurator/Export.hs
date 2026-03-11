{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Configurator.Export
-- Description : Pretty printers and exporters for configurator 'Config's.
-- Copyright   : (c) Justin Le 2016
-- License     : BSD3
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Pretty printers and exporters for 'Config's from the /configurator/
-- library, in "Data.Configurator".
--
-- All results are intended to be valid parsing files in the configuration
-- file syntax of the library.
--
-- For a full round trip:
--
-- @
-- main = do
--   cfg <- load [Required "config.cfg"]
--   writeConf "config.cfg" cfg
-- @
--
-- This should load the config file, parse it, and then re-export it,
-- rewriting the original config file.  The result should be an identical
-- configuration file (with keys potentially re-arranged and re-sorted,
-- comments removed, etc.)
--
-- Print/export your own dynmically generated configuration files by
-- manipulating the @'HashMap' 'Name' 'Value'@ that a 'Config' gives with
-- 'getMap'.
--
-- Sample output:
--
-- > foo {
-- >     bar {
-- >         baz1  = true
-- >         baz2  = [1, 0.6, "hello", true]
-- >     }
-- >     aardvark  = "banana"
-- >     monkey    = [true, false, 1.9e-3]
-- >     zebra     = 24
-- > }
-- >
-- > foo2 {
-- >     bar = 8.1e-8
-- > }
-- >
-- > apple   = ["cake", true]
-- > orange  = 8943
--
-- Further configuration on sorting of keys, displaying of bools and
-- floats, etc. is possible by passing in custom 'ConfStyle' values.
--

module Data.Configurator.Export (
  -- * Default styles
  -- ** Pretty printing / Exporting
    renderConf          -- :: Config -> IO String
  , displayConf         -- :: Config -> IO ()
  , writeConf           -- :: FilePath -> Config -> IO ()
  , renderHashMap       -- :: HashMap Name Value -> String
  -- ** Doc
  , confDoc             -- :: Config -> IO Doc
  , hashMapDoc          -- :: HashMap Name Value -> Doc
  -- * With styles
  -- ** Describing styles
  , confStyle           -- :: ConfStyle
  , ConfStyle(..)
  , AlignStyle(..)
  , BraceStyle(..)
  , BoolStyle(..)
  , KeyType(..)
  -- ** Pretty printing / Exporting
  , renderConf'         -- :: ConfStyle -> Config -> IO String
  , displayConf'        -- :: ConfStyle -> Config -> IO ()
  , writeConf'          -- :: ConfStyle -> FilePath -> Config -> IO ()
  , renderHashMap'      -- :: ConfStyle -> HashMap Name Value -> String
  -- ** Doc
  , confDoc'            -- :: ConfStyle -> Config -> IO Doc
  , hashMapDoc'         -- :: ConfSTyle -> HashMap Name Value -> Doc
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Bool.Compat
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Foldable           (toList)
import           Data.Function
import           Data.HashMap.Strict     (HashMap)
import           Data.List               (sortBy)
import           Data.List.NonEmpty      (NonEmpty(..))
import           Data.Monoid
import           Data.Ord
import           Data.Ratio
import           Data.Text               (Text)
import           Numeric.Compat
import           Prelude                 ()
import           Prelude.Compat
import           Text.PrettyPrint        (Doc, (<+>), ($+$), ($$))
import qualified Data.HashMap.Strict     as HM
import qualified Data.List.NonEmpty      as NE
import qualified Data.Text               as T
import qualified Text.PrettyPrint        as P

data HashMapTree k v = HMT { getHashMapTree :: HashMap k (Either v (HashMapTree k v))
                           }
  deriving (Show, Eq)

-- | The type of structure that the key contains.  Used for sorting.
data KeyType  = KeyGroup
                -- ^ Key is associated with a group.
              | KeyValue
                -- ^ Key is associated with a single value.
  deriving (Show, Eq, Ord, Read)

-- | The style of boolean literals display 'Bool's as.  Both are accepted
-- by configurator's parser.
data BoolStyle  = OnOff
                  -- ^ @on@ and @off@
                | TrueFalse
                  -- ^ @true@ and @false@
  deriving (Show, Eq, Ord, Read)

-- | Alignment style of equals signs on contiguous sets of keys of values.
data AlignStyle = NoAlign
                  -- ^ Don't align equals signs at all.
                | AlignAny
                  -- ^ Align them to the longest key.
                | AlignOn Int
                  -- ^ Align to the longest key, but make sure the
                  -- identation is a multiple of this number.
  deriving (Show, Eq, Ord, Read)

-- | Placement style of opening braces (curly brackets) for groups.
data BraceStyle = SameLineBrace
                  -- ^ Opening braces go on the same line as the key name.
                | NewLineBrace
                  -- ^ Opening braces go on a new line after the key name.
  deriving (Show, Eq, Ord, Read)

-- | Style options for pretty-printing the contents of a 'Config'.
-- Sensible defaults are given as 'confStyle'; it's recommended that you
-- start with 'confStyle' as a default and use record syntax to modify it
-- to what you want.  See 'confStyle' for more details.
data ConfStyle
    = ConfStyle { confStyleIndent     :: Int
                  -- ^ Number of columns to indent each nested group.
                , confStyleAlign      :: AlignStyle
                  -- ^ Style of aligning the equals signs for keys with
                  -- values.
                , confStyleBraceStyle :: BraceStyle
                  -- ^ Style of opening brace (curly bracket) placement.
                , confStyleBoolStyle  :: BoolStyle
                  -- ^ Style of displaying 'Bool's as boolean literals.
                , confStyleForceDec   :: Bool
                  -- ^ Force full decimals to be rendered, instead of
                  -- truncating to scientific notation for numbers less
                  -- than @0.1@.
                , confStyleShowInts   :: Bool
                  -- ^ Whether or not to show "whole numbers" as integer
                  -- literals (without the trailing @.0@)
                , confStyleSortBy     :: (Name, KeyType) -> (Name, KeyType) -> Ordering
                  -- ^ Function to sort keys by, with information on
                  -- whether or not the key contains a group or a single
                  -- value.
                , confStyleGroupSep   :: Int
                  -- ^ Newline seperators between groups.
                , confStyleValueSep   :: Int
                  -- ^ Newline seperators between chunks of contiguous
                  -- values.
                , confStyleTopSep     :: Int
                  -- ^ Newline seperators between groups and chunks of
                  -- contiguous values at the top level.
                }

-- | Sensible defaults for a 'ConfStyle':
--
-- @
-- confStyle :: 'ConfStyle'
-- confStyle = 'ConfStyle' { confStyleIndent     = 4
--                       , confStyleAlign      = 'AlignOn' 2
--                       , confStyleBraceStyle = 'SameLineBrace'
--                       , confStyleBoolStyle  = 'TrueFalse'
--                       , confStyleForceDec   = False
--                       , confStyleShowInts   = True
--                         -- sort by "type" of key, then sort alphabetically
--                       , confStyleSortBy     = 'comparing' snd '<>' comparing fst
--                       , confStyleGroupSep   = 0
--                       , confStyleValueSep   = 0
--                       , confStyleTopSep     = 1
--                       }
-- @
--
-- It's recommended that you create 'ConfStyle's by modifying this value
-- using record syntax rather than create your own from scratch:
--
-- @
-- myStyle = 'confStyle' { confStyleBraceStyle = 'NewLineBrace'
--                     , confStyleBoolStyle  = 'OnOff'
--                     }
-- @
--
confStyle :: ConfStyle
confStyle = ConfStyle { confStyleIndent     = 4
                      , confStyleAlign      = AlignOn 2
                      , confStyleBraceStyle = SameLineBrace
                      , confStyleBoolStyle  = TrueFalse
                      , confStyleForceDec   = False
                      , confStyleShowInts   = True
                      , confStyleSortBy     = comparing snd <> comparing fst
                      , confStyleGroupSep   = 0
                      , confStyleValueSep   = 0
                      , confStyleTopSep     = 1
                      }

-- | Render/pretty print the current contents of the given 'Config' to a 'String'.
renderConf :: Config -> IO String
renderConf = renderConf' confStyle

-- | Render/pretty print the current contents of the given 'Config' to
-- a 'String', providing a 'ConfStyle' with the rendering style.
renderConf' :: ConfStyle -> Config -> IO String
renderConf' s c = renderHashMap' s <$> getMap c

-- | Render/pretty print the contents of a 'HashMap' of keys and 'Value's
-- to a 'String'.
renderHashMap :: HashMap Name Value -> String
renderHashMap = renderHashMap' confStyle

-- | Render/pretty print the contents of a 'HashMap' of keys and 'Value's
-- to a 'String', providing a 'ConfStyle' with the rendering style.
renderHashMap' :: ConfStyle -> HashMap Name Value -> String
renderHashMap' s = P.render . hashMapDoc' s

-- | Print out a pretty printed rendering of the current contents of the
-- 'Config' to stdout.
displayConf :: Config -> IO ()
displayConf = displayConf' confStyle

-- | Print out a pretty printed rendering of the current contents of the
-- 'Config' to stdout, providing a 'ConfStyle' with the rendering style.
displayConf' :: ConfStyle -> Config -> IO ()
displayConf' s c = putStrLn =<< renderConf' s c

-- | Write the current contents of the given 'Config' to the given
-- 'FilePath'.
writeConf :: FilePath -> Config -> IO ()
writeConf = writeConf' confStyle

-- | Write the current contents of the given 'Config' to the given
-- 'FilePath', providing a 'ConfStyle' with the rendering style.
writeConf' :: ConfStyle -> FilePath -> Config -> IO ()
writeConf' s fp = writeFile fp <=< renderConf' s

-- | Convert the current contents of the given 'Config' to a 'Doc' from the
-- /pretty/ package.  This allows more fine-grained control over printing
-- it.
confDoc :: Config -> IO P.Doc
confDoc c = hashMapDoc <$> getMap c

-- | Convert the current contents of the given 'Config' to a 'Doc' from the
-- /pretty/ package.  This allows more fine-grained control over printing
-- it.  Takes a 'ConfStyle' with the rendering style.
confDoc' :: ConfStyle -> Config -> IO P.Doc
confDoc' s c = hashMapDoc' s <$> getMap c

-- | Convert a 'HashMap' of keys and 'Value's into a 'Doc', from the
-- /pretty/ package.  This allows more fine-grained control over printing
-- it.
--
-- Expects keys to be in the format exported from a 'Config' using
-- 'getMap'.  "foo.bar.baz.x" is "x" in group "baz" in group "bar" in group
-- "foo", etc.
hashMapDoc :: HashMap Name Value -> P.Doc
hashMapDoc = hashMapDoc' confStyle

-- | Convert a 'HashMap' of keys and 'Value's into a 'Doc', from the
-- /pretty/ package.  This allows more fine-grained control over printing
-- it.  Takes a 'ConfStyle' with the rendering style.
--
-- Expects keys to be in the format exported from a 'Config' using
-- 'getMap'.  "foo.bar.baz.x" is "x" in group "baz" in group "bar" in group
-- "foo", etc.
hashMapDoc' :: ConfStyle -> HashMap Name Value -> P.Doc
hashMapDoc' s = hmtToDoc s . toHashMapTree "."


toHashMapTree
    :: forall v.
       Text
    -> HashMap Text v
    -> HashMapTree Text v
toHashMapTree sep = go
  where
    go :: HashMap Text v -> HashMapTree Text v
    go = HMT
       . HM.fromList
       . map condense
       . NE.groupAllWith (NE.head . fst)
       . (map . first) (NE.fromList . T.splitOn sep)
       . HM.toList
    condense :: NonEmpty (NonEmpty Text, v) -> (Text, Either v (HashMapTree Text v))
    condense kvs@((topKey :| subKeys, v1) :| _) = (topKey, hm)
      where
        hm =  case subKeys of
                [] -> Left v1           -- kvs' is [], only one value
                _  -> Right
                    . go
                    . HM.fromList
                    . (map . first) (T.intercalate sep . NE.tail)
                    $ toList kvs

hmtToDoc :: ConfStyle -> HashMapTree Text Value -> P.Doc
hmtToDoc ConfStyle{..} = go True
  where
    v2d :: Value -> Doc
    v2d = valueToDoc confStyleBoolStyle confStyleForceDec confStyleShowInts
    toKeyType :: Either a b -> KeyType
    toKeyType (Left  _) = KeyValue
    toKeyType (Right _) = KeyGroup
    addSep 0 = id
    addSep s = allButLast ($$ P.text (replicate (s - 1) '\n'))
    go :: Bool -> HashMapTree Text Value -> P.Doc
    go isTop = P.vcat
             . addSep (if isTop then confStyleTopSep else confStyleValueSep)
             . map keysToDoc
             . NE.groupWith (toKeyType . snd)
             . sortBy (confStyleSortBy `on` second toKeyType)
             . HM.toList
             . getHashMapTree
      where
        keysToDoc :: NonEmpty (Text, Either Value (HashMapTree Text Value)) -> P.Doc
        keysToDoc l@((_, Left _) :| _)
            = P.vcat [ keyToDoc k v | (k, Left v) <- toList l ]
          where
            maxKeyLength = maximum $ fmap (T.length . fst) l
            keyToDoc k v = P.hsep [ nameToDoc k
                                  , P.equals
                                  , v2d v
                                  ]
            nameToDoc = case confStyleAlign of
                          NoAlign   -> P.text . T.unpack
                          AlignAny  -> P.text . T.unpack . T.justifyLeft maxKeyLength ' '
                          AlignOn n -> let res | n <= 0    = 1
                                               | otherwise = n
                                           maxLen = (maxKeyLength + 1) + (1 - (maxKeyLength + 1)) `mod` res
                                       in  P.text . T.unpack . T.justifyLeft (maxLen - 1) ' '
        keysToDoc l@((_, Right _) :| _)
            = P.vcat . addSep (if isTop then confStyleTopSep else confStyleGroupSep)
            $ [ groupToDoc n g | (n, Right g) <- toList l ]
          where
            groupToDoc k g = case confStyleBraceStyle of
                               SameLineBrace -> P.text (T.unpack k) <+> P.lbrace
                               NewLineBrace  -> P.text (T.unpack k) $+$ P.lbrace
                         $+$ P.nest confStyleIndent (go False g)
                         $+$ P.rbrace

valueToDoc :: BoolStyle -> Bool -> Bool -> Value -> Doc
valueToDoc bs forceDec showInts = go
  where
    go (Bool b)   = withBoolStyle bs b
    go (String t) = P.text $ show t
    go (Number n)
      | showInts && denominator n == 1
          = P.integer $ round n
      | forceDec
          = P.text . ($"") . showFFloatAlt Nothing $ (fromRational n :: Double)
      | otherwise
          = P.double $ fromRational n
    go (List vs)  = P.brackets
                  . P.hsep
                  . P.punctuate P.comma
                  $ map go vs
    withBoolStyle :: BoolStyle -> Bool -> P.Doc
    withBoolStyle OnOff     = bool "off" "on"
    withBoolStyle TrueFalse = bool "false" "true"

allButLast :: (a -> a) -> [a] -> [a]
allButLast f = go
  where
    go []     = []
    go [x]    = [x]
    go (x:xs) = f x : go xs
