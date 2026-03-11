{-# LANGUAGE OverloadedStrings    #-}
{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling and unmarshaling of 'Caption' elements.
-}
module Text.Pandoc.Lua.Marshal.Caption
  ( peekCaption
  , peekCaptionFuzzy
  , pushCaption
    -- * Constructor
  , mkCaption
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad ((<$!>))
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import HsLua
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Block
  ( peekBlocksFuzzy, pushBlocks )
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Inline
  ( peekInlinesFuzzy, pushInlines )
import Text.Pandoc.Definition

-- | Caption object type.
typeCaption :: LuaError e => DocumentedType e Caption
typeCaption = deftype "Caption"
  [ operation Eq $ lambda
    ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
    <#> parameter (optional . peekCaption) "Caption" "a" ""
    <#> parameter (optional . peekCaption) "Caption" "b" ""
    =#> functionResult pushBool "boolean" "whether the two are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> udparam typeCaption "x" ""
    =#> functionResult pushString "string" "native Haskell representation"
  , operation (CustomOperation "__tojson") $ lambda
    ### liftPure encode
    <#> udparam typeCaption "self" ""
    =#> functionResult pushLazyByteString "string" "JSON representation"
  ]
  [ property' "short"
    "Inlines|nil"
    "short caption used to describe the object"
      (maybe pushnil pushInlines, \(Caption short _) -> short)
      (peekNilOr peekInlinesFuzzy, \(Caption _ long) shrt -> Caption shrt long)
  , property "long" "full caption text"
      (pushBlocks, \(Caption _ long) -> long)
      (peekBlocksFuzzy, \(Caption short _) long -> Caption short long)
  , method $ defun "clone"
    ### return
    <#> parameter peekCaption "Caption" "capt" ""
    =#> functionResult pushCaption "Caption" "cloned Caption element"
  ]

-- | Push Caption element
pushCaption :: LuaError e => Pusher e Caption
pushCaption = pushUD typeCaption

-- | Peek Caption element from userdata.
peekCaption :: LuaError e => Peeker e Caption
peekCaption = peekUD typeCaption

-- | Peek Caption element from a table.
peekCaptionTable :: LuaError e => Peeker e Caption
peekCaptionTable idx = do
  short <- optional $ peekFieldRaw peekInlinesFuzzy "short" idx
  long <- peekFieldRaw peekBlocksFuzzy "long" idx
  return $! Caption short long

peekCaptionFuzzy :: LuaError e => Peeker e Caption
peekCaptionFuzzy = retrieving "Caption" . \idx -> do
      peekCaption idx
  <|> peekCaptionTable idx
  <|> (Caption Nothing <$!> peekBlocksFuzzy idx)
  <|> (failPeek =<<
       typeMismatchMessage "Caption, list of Blocks, or compatible element" idx)

-- | Constructor for 'Caption'.
mkCaption :: LuaError e => DocumentedFunction e
mkCaption = defun "Caption"
  ### (\mLong short ->
         let long = fromMaybe mempty mLong
         in pure (Caption short long))
  <#> opt (parameter peekBlocksFuzzy "Blocks" "long" "full caption")
  <#> opt (parameter peekInlinesFuzzy "Inlines" "short" "short summary caption")
  =#> functionResult pushCaption "Caption" "new Caption object"
  #? "Creates a new Caption object."
