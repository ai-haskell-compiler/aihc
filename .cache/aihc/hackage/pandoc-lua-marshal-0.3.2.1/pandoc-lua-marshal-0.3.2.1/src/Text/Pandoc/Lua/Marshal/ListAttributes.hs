{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions and constructor for 'ListAttributes'
values.
-}
module Text.Pandoc.Lua.Marshal.ListAttributes
  ( typeListAttributes
  , peekListAttributes
  , pushListAttributes
  , mkListAttributes
  , peekListNumberDelim
  , pushListNumberDelim
  , peekListNumberStyle
  , pushListNumberStyle
  ) where

import Control.Applicative (optional)
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import HsLua
import Text.Pandoc.Definition
  ( ListAttributes, ListNumberStyle (..), ListNumberDelim (..))

-- | 'ListAttributes' Lua object type.
typeListAttributes :: LuaError e => DocumentedType e ListAttributes
typeListAttributes = deftype "ListAttributes"
  [ operation Eq $ lambda
    ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
    <#> parameter (optional . peekListAttributes) "a" "ListAttributes" ""
    <#> parameter (optional . peekListAttributes) "b" "ListAttributes" ""
    =#> functionResult pushBool "boolean" "whether the two are equal"
  , operation (CustomOperation "__tojson") $ lambda
    ### liftPure encode
    <#> udparam typeListAttributes "self" ""
    =#> functionResult pushLazyByteString "string" "JSON representation"
  ]
  [ property "start" "number of the first list item"
      (pushIntegral, \(start,_,_) -> start)
      (peekIntegral, \(_,style,delim) -> (,style,delim))
  , property "style" "style used for list numbering"
      (pushListNumberStyle, \(_,style,_) -> style)
      (peekListNumberStyle, \(start,_,delim) -> (start,,delim))
  , property "delimiter" "delimiter of list numbers"
      (pushListNumberDelim, \(_,_,delim) -> delim)
      (peekListNumberDelim, \(start,style,_) -> (start,style,))
  , method $ defun "clone"
    ### return
    <#> udparam typeListAttributes "a" ""
    =#> functionResult (pushUD typeListAttributes) "ListAttributes"
          "cloned ListAttributes value"
  ]

-- | Pushes a 'ListAttributes' value as userdata object.
pushListAttributes :: LuaError e => Pusher e ListAttributes
pushListAttributes = pushUD typeListAttributes

-- | Retrieve a 'ListAttributes' triple, either from userdata or from a
-- Lua tuple.
peekListAttributes :: LuaError e => Peeker e ListAttributes
peekListAttributes = retrieving "ListAttributes" . choice
  [ peekUD typeListAttributes
  , peekTriple peekIntegral peekRead peekRead
  ]

-- | Constructor for a new 'ListAttributes' value.
mkListAttributes :: LuaError e => DocumentedFunction e
mkListAttributes = defun "ListAttributes"
  ### liftPure3 (\mstart mstyle mdelim ->
                   ( fromMaybe 1 mstart
                   , fromMaybe DefaultStyle mstyle
                   , fromMaybe DefaultDelim mdelim
                   ))
  <#> opt (integralParam "start" "number of the first list item")
  <#> opt (parameter peekRead "string" "style" "style used for list numbering")
  <#> opt (parameter peekRead "string" "delimiter" "delimiter of list numbers")
  =#> udresult typeListAttributes "new ListAttributes object"
  #? "Creates a new ListAttributes object."

-- | Pushes a 'ListNumberDelim' value as string.
pushListNumberDelim :: Pusher e ListNumberDelim
pushListNumberDelim = pushString . show
{-# INLINE pushListNumberDelim #-}

-- | Retrieves a 'ListNumberDelim' value from a string.
peekListNumberDelim :: Peeker e ListNumberDelim
peekListNumberDelim = peekRead
{-# INLINE peekListNumberDelim #-}

-- | Pushes a 'ListNumberStyle' value as string.
pushListNumberStyle :: Pusher e ListNumberStyle
pushListNumberStyle = pushString . show
{-# INLINE pushListNumberStyle #-}

-- | Retrieves a 'ListNumberStyle' value from a string.
peekListNumberStyle :: Peeker e ListNumberStyle
peekListNumberStyle = peekRead
{-# INLINE peekListNumberStyle #-}
