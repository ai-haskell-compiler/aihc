{-# LANGUAGE OverloadedStrings    #-}
{- |
Copyright               : © 2021-2026 Albert Krewinkel
SPDX-License-Identifier : MIT
Maintainer              : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling/unmarshaling functions and constructor for 'Citation' values.
-}
module Text.Pandoc.Lua.Marshal.Citation
  ( -- * Citation
    peekCitation
  , pushCitation
  , typeCitation
  , mkCitation
  ) where

import Control.Applicative (optional)
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import HsLua as Lua
import Text.Pandoc.Definition (Citation (..))
import Text.Pandoc.Lua.Marshal.CitationMode (peekCitationMode, pushCitationMode)
import {-# SOURCE #-} Text.Pandoc.Lua.Marshal.Inline
  ( peekInlinesFuzzy, pushInlines )

-- | Pushes a Citation value as userdata object.
pushCitation :: LuaError e
             => Pusher e Citation
pushCitation = pushUD typeCitation
{-# INLINE pushCitation #-}

-- | Retrieves a Citation value.
peekCitation :: LuaError e
             => Peeker e Citation
peekCitation = peekUD  typeCitation
{-# INLINE peekCitation #-}

-- | Citation object type.
typeCitation :: LuaError e
             => DocumentedType e Citation
typeCitation = deftype "Citation"
  [ operation Eq $ lambda
    ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
    <#> parameter (optional . peekCitation) "Citation" "a" ""
    <#> parameter (optional . peekCitation) "Citation" "b" ""
    =#> functionResult pushBool "boolean" "true iff the citations are equal"

  , operation Tostring $ lambda
    ### liftPure show
    <#> parameter peekCitation "Citation" "citation" ""
    =#> functionResult pushString "string" "native Haskell representation"
  , operation (CustomOperation "__tojson") $ lambda
    ### liftPure encode
    <#> udparam typeCitation "self" ""
    =#> functionResult pushLazyByteString "string" "JSON representation"
  ]
  [ property "id" "citation ID / key"
      (pushText, citationId)
      (peekText, \citation cid -> citation{ citationId = cid })
  , property "mode" "citation mode"
      (pushCitationMode, citationMode)
      (peekCitationMode, \citation mode -> citation{ citationMode = mode })
  , property "prefix" "citation prefix"
      (pushInlines, citationPrefix)
      (peekInlinesFuzzy, \citation prefix -> citation{ citationPrefix = prefix })
  , property "suffix" "citation suffix"
      (pushInlines, citationSuffix)
      (peekInlinesFuzzy, \citation suffix -> citation{ citationSuffix = suffix })
  , property "note_num" "note number"
      (pushIntegral, citationNoteNum)
      (peekIntegral, \citation noteNum -> citation{ citationNoteNum = noteNum })
  , property "hash" "hash number"
      (pushIntegral, citationHash)
      (peekIntegral, \citation hash -> citation{ citationHash = hash })
  , method $ defun "clone"
    ### return
    <#> udparam typeCitation "obj" ""
    =#> functionResult pushCitation "Citation" "copy of obj"
  ]
{-# INLINABLE typeCitation #-}

-- | Constructor function for 'Citation' elements.
mkCitation :: LuaError e => DocumentedFunction e
mkCitation = defun "Citation"
  ### (\cid mode mprefix msuffix mnote_num mhash ->
         cid `seq` mode `seq` mprefix `seq` msuffix `seq`
         mnote_num `seq` mhash `seq` return $! Citation
           { citationId = cid
           , citationMode = mode
           , citationPrefix = fromMaybe mempty mprefix
           , citationSuffix = fromMaybe mempty msuffix
           , citationNoteNum = fromMaybe 0 mnote_num
           , citationHash = fromMaybe 0 mhash
           })
  <#> textParam "id" "citation ID (e.g. BibTeX key)"
  <#> parameter peekCitationMode "CitationMode" "mode" "citation rendering mode"
  <#> opt (parameter peekInlinesFuzzy "Inlines" "prefix" "")
  <#> opt (parameter peekInlinesFuzzy "Inlines" "suffix" "")
  <#> opt (integralParam "note_num" "note number")
  <#> opt (integralParam "hash"     "hash number")
  =#> functionResult pushCitation "Citation" "new citation object"
  #? "Creates a single citation."
