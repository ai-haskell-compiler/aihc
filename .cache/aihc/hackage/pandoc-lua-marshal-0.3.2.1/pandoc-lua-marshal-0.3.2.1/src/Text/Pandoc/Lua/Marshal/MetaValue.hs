{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions of 'MetaValue' elements.
-}
module Text.Pandoc.Lua.Marshal.MetaValue
  ( peekMetaValue
  , pushMetaValue
  , metaValueConstructors
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad ((<$!>))
import HsLua
import Text.Pandoc.Lua.Marshal.Block
  ( peekBlock, peekBlocks, peekBlocksFuzzy, pushBlocks )
import Text.Pandoc.Lua.Marshal.Inline
  ( peekInline, peekInlines, peekInlinesFuzzy, pushInlines )
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Definition (MetaValue (..))
import qualified Data.Text as T

-- | Push a 'MetaValue' element to the top of the Lua stack.
pushMetaValue :: LuaError e => Pusher e MetaValue
pushMetaValue = \case
  MetaBlocks blcks  -> pushBlocks blcks
  MetaBool bool     -> pushBool bool
  MetaInlines inlns -> pushInlines inlns
  MetaList metalist -> pushPandocList pushMetaValue metalist
  MetaMap metamap   -> pushMap pushText pushMetaValue metamap
  MetaString t      -> pushText t

-- | Retrieves the value at the given stack index as 'MetaValue'.
peekMetaValue :: forall e. LuaError e => Peeker e MetaValue
peekMetaValue = retrieving "MetaValue" . \idx -> do
  -- Get the contents of an AST element.

  liftLua (ltype idx) >>= \case
    TypeBoolean -> MetaBool <$!> peekBool idx

    TypeString  -> MetaString <$!> peekText idx

    TypeNumber  -> MetaString . T.pack <$>
      (liftLua (isinteger idx) >>= \case
          False -> show <$!> peekRealFloat @Double idx
          True  -> show <$!> peekIntegral @Prelude.Integer idx)

    TypeUserdata -> -- Allow singleton Inline or Block elements
      (MetaInlines . (:[]) <$!> peekInline idx) <|>
      (MetaBlocks . (:[]) <$!> peekBlock idx)

    TypeTable   -> optional (getName' idx) >>= \case
      Just "Inlines" -> MetaInlines <$!> peekInlinesFuzzy idx
      Just "Blocks"  -> MetaBlocks  <$!> peekBlocksFuzzy idx
      Just "List"    -> MetaList <$!> peekList peekMetaValue idx
      _ -> do
        -- no meta value tag given, try to guess.
        len <- liftLua $ rawlen idx
        if len <= 0
          then MetaMap <$!> peekMap peekText peekMetaValue idx
          else  (MetaInlines <$!> peekInlines idx)
            <|> (MetaBlocks <$!> peekBlocks idx)
            <|> (MetaList <$!> peekList peekMetaValue idx)

    _ -> failPeek "could not get meta value"

 where
  getName' idx = liftLua (getmetafield idx "__name") >>= \case
    TypeNil -> failPeek "no name"
    _ -> peekName idx `lastly` pop 1


-- | Constructor functions for 'MetaValue' elements.
metaValueConstructors :: LuaError e => [DocumentedFunction e]
metaValueConstructors =
  [ defun "MetaBlocks"
    ### liftPure MetaBlocks
    <#> parameter peekBlocksFuzzy "Blocks" "content" "block content"
    =#> functionResult pushMetaValue "Blocks" "list of Block elements"
    #? T.unlines
    [ "Creates a value to be used as a MetaBlocks value in meta"
    , "data; creates a copy of the input list via `pandoc.Blocks`,"
    , "discarding all non-list keys."
    ]

  , defun "MetaBool"
    ### liftPure MetaBool
    <#> boolParam "bool" "true or false"
    =#> functionResult pushMetaValue "boolean" "input, unchanged"

  , defun "MetaInlines"
    ### liftPure MetaInlines
    <#> parameter peekInlinesFuzzy "Inlines" "inlines" "inline elements"
    =#> functionResult pushMetaValue "Inlines" "list of Inline elements"
    #? T.unlines
    [ "Creates a value to be used as a MetaInlines value in meta"
    , "data; creates a copy of the input list via `pandoc.Inlines`,"
    , "discarding all non-list keys."
    ]

  , defun "MetaList"
    ### liftPure MetaList
    <#> parameter (peekList peekMetaValue) "MetaValue|{MetaValue,...}"
          "values" "value, or list of values"
    =#> functionResult pushMetaValue "List" "list of meta values"
    #? T.unlines
    [ "Creates a value to be used as a MetaList in meta data;"
    , "creates a copy of the input list via `pandoc.List`,"
    , "discarding all non-list keys."
    ]

  , defun "MetaMap"
    ### liftPure MetaMap
    <#> parameter (peekMap peekText peekMetaValue) "table" "key_value_map"
          "a string-indexed map of meta values"
    =#> functionResult pushMetaValue "table" "map of meta values"
    #? T.unlines
    [ "Creates a value to be used as a MetaMap in meta data; creates"
    , "a copy of the input table, keeping only pairs with string"
    , "keys and discards all other keys."
    ]

  , defun "MetaString"
    ### liftPure MetaString
    <#> textParam "s" "string value"
    =#> functionResult pushMetaValue "string" "unchanged input"
    #? T.unlines
    [ "Creates a value to be used as a MetaString in meta data; this"
    , "is the identity function for boolean values and exists only"
    , "for completeness."
    ]
  ]
