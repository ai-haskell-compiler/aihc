{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{- |
Module      : Text.Pandoc.Lua.Marshal.Attr
Copyright   : © 2017-2021 Albert Krewinkel, John MacFarlane
License     : MIT

Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Helpers to make pandoc's Attr elements usable in Lua, and to get objects
back into Haskell.
-}
module Text.Pandoc.Lua.Marshal.Attr
  ( typeAttr
  , peekAttr
  , pushAttr
  , typeAttributeList
  , pushAttributeList
  , peekAttributeList
  , mkAttr
  , mkAttributeList
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad ((<$!>))
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import HsLua
import HsLua.Marshalling.Peekers (peekIndexRaw)
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Safe (atMay)
import Text.Pandoc.Definition (Attr, nullAttr)

import qualified Data.Text as T

-- | Attr object type.
typeAttr :: LuaError e => DocumentedType e Attr
typeAttr = deftype "Attr"
  [ operation Eq $ lambda
    ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
    <#> parameter (optional . peekAttr) "a" "Attr" ""
    <#> parameter (optional . peekAttr) "b" "Attr" ""
    =#> functionResult pushBool "boolean" "whether the two are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> parameter peekAttr "Attr" "attr" ""
    =#> functionResult pushString "string" "native Haskell representation"
  , operation (CustomOperation "__tojson") $ lambda
    ### liftPure encode
    <#> udparam typeAttr "self" ""
    =#> functionResult pushLazyByteString "string" "JSON representation"
  ]
  [ property "identifier" "element identifier"
      (pushText, \(ident,_,_) -> ident)
      (peekText, \(_,cls,kv) -> (,cls,kv))
  , property "classes" "element classes"
      (pushPandocList pushText, \(_,classes,_) -> classes)
      (peekList peekText, \(ident,_,kv) -> (ident,,kv))
  , property "attributes" "various element attributes"
      (pushAttributeList, \(_,_,attribs) -> attribs)
      (peekAttributeList, \(ident,cls,_) -> (ident,cls,))
  , method $ defun "clone"
    ### return
    <#> parameter peekAttr "attr" "Attr" ""
    =#> functionResult pushAttr "Attr" "new Attr element"
  , readonly "tag" "element type tag (always 'Attr')"
      (pushText, const "Attr")

  , alias "t" "alias for `tag`" ["tag"]
  ]

-- | Pushes an 'Attr' value as @Attr@ userdata object.
pushAttr :: LuaError e => Pusher e Attr
pushAttr = pushUD typeAttr

-- | Retrieves an associated list of attributes from a table or an
-- @AttributeList@ userdata object.
peekAttributeList :: LuaError e => Peeker e [(Text,Text)]
peekAttributeList idx = liftLua (ltype idx) >>= \case
  TypeUserdata -> peekUD typeAttributeList idx
  TypeTable    -> liftLua (rawlen idx) >>= \case
    0 -> peekKeyValuePairs peekText peekText idx
    _ -> peekList (peekPair peekText peekText) idx
  _            -> failPeek "unsupported type"

-- | Pushes an associated list of attributes as @AttributeList@ userdata
-- object.
pushAttributeList :: LuaError e => Pusher e [(Text, Text)]
pushAttributeList = pushUD typeAttributeList

-- | Constructor functions for 'AttributeList' elements.
typeAttributeList :: LuaError e => DocumentedType e [(Text, Text)]
typeAttributeList = deftype "AttributeList"
  [ operation Eq $ lambda
    ### liftPure2 (\a b -> Just True == ((==) <$> a <*> b))
    <#> parameter (optional . peekAttributeList) "a" "any" ""
    <#> parameter (optional . peekAttributeList) "b" "any" ""
    =#> functionResult pushBool "boolean" "whether the two are equal"

  , operation Index $ lambda
    ### liftPure2 lookupKey
    <#> udparam typeAttributeList "t" "attributes list"
    <#> parameter peekKey "string|integer" "key" "lookup key"
    =#> functionResult (maybe pushnil pushAttribute) "string|table"
          "attribute value"

  , operation Newindex $ lambda
    ### setKey
    <#> udparam typeAttributeList "t" "attributes list"
    <#> parameter peekKey "string|integer" "key" "lookup key"
    <#> opt (parameter peekAttribute "string|nil" "value" "new value")
    =#> []

  , operation Len $ lambda
    ### liftPure length
    <#> udparam typeAttributeList "t" "attributes list"
    =#> functionResult pushIntegral "integer" "number of attributes in list"

  , operation Pairs $ lambda
    ### pushIterator (\(k, v) -> 2 <$ pushText k <* pushText v)
    <#> udparam typeAttributeList "t" "attributes list"
    =?> "iterator triple"

  , operation Tostring $ lambda
    ### liftPure show
    <#> udparam typeAttributeList "t" "attributes list"
    =#> functionResult pushString "string" ""
  ]
  []

data Key = StringKey Text | IntKey Int

peekKey :: Peeker e (Maybe Key)
peekKey idx = liftLua (ltype idx) >>= \case
  TypeNumber -> Just . IntKey <$!> peekIntegral idx
  TypeString -> Just . StringKey <$!> peekText idx
  _          -> return Nothing

data Attribute
  = AttributePair (Text, Text)
  | AttributeValue Text

pushAttribute :: LuaError e => Pusher e Attribute
pushAttribute = \case
  (AttributePair kv) -> pushPair pushText pushText kv
  (AttributeValue v) -> pushText v

-- | Retrieve an 'Attribute'.
peekAttribute :: LuaError e => Peeker e Attribute
peekAttribute idx = (AttributeValue <$!> peekText idx)
  <|> (AttributePair <$!> peekPair peekText peekText idx)

lookupKey :: [(Text,Text)] -> Maybe Key -> Maybe Attribute
lookupKey !kvs = \case
  Just (StringKey str) -> AttributeValue <$!> lookup str kvs
  Just (IntKey n)      -> AttributePair <$!> atMay kvs (n - 1)
  Nothing              -> Nothing

setKey :: forall e. LuaError e
       => [(Text, Text)] -> Maybe Key -> Maybe Attribute
       -> LuaE e ()
setKey kvs mbKey mbValue = case mbKey of
  Just (StringKey str) ->
    case break ((== str) . fst) kvs of
      (prefix, _:suffix) -> case mbValue of
        Nothing -> setNew $ prefix ++ suffix
        Just (AttributeValue value) -> setNew $ prefix ++ (str, value):suffix
        _ -> failLua "invalid attribute value"
      _  -> case mbValue of
        Nothing -> return ()
        Just (AttributeValue value) -> setNew (kvs ++ [(str, value)])
        _ -> failLua "invalid attribute value"
  Just (IntKey idx) ->
    case splitAt (idx - 1) kvs of
      (prefix, (k,_):suffix) -> setNew $ case mbValue of
        Nothing -> prefix ++ suffix
        Just (AttributePair kv) -> prefix ++ kv : suffix
        Just (AttributeValue v) -> prefix ++ (k, v) : suffix
      (prefix, []) -> case mbValue of
        Nothing -> setNew prefix
        Just (AttributePair kv) -> setNew $ prefix ++ [kv]
        _ -> failLua $ "trying to set an attribute key-value pair, "
             ++ "but got a single string instead."

  _  -> failLua "invalid attribute key"
  where
    setNew :: [(Text, Text)] -> LuaE e ()
    setNew new =
      putuserdata (nthBottom 1) (udName @e typeAttributeList) new >>= \case
        True -> return ()
        False -> failLua "failed to modify attributes list"

-- | Retrieves an 'Attr' value from a string, a table, or an @Attr@
-- userdata object. A string is used as an identifier; a table is either
-- an HTML-like set of attributes, or a triple containing the
-- identifier, classes, and attributes.
peekAttr :: LuaError e => Peeker e Attr
peekAttr idx = retrieving "Attr" $ liftLua (ltype idx) >>= \case
  TypeString -> (,[],[]) <$!> peekText idx -- treat string as ID
  TypeUserdata -> peekUD typeAttr idx
  TypeTable -> peekAttrTable idx
  x -> liftLua . failLua $ "Cannot get Attr from " ++ show x

-- | Helper function which gets an Attr from a Lua table.
peekAttrTable :: LuaError e => Peeker e Attr
peekAttrTable idx = do
  len' <- liftLua $ rawlen idx
  let peekClasses = peekList peekText
  if len' > 0
    then do
      ident <- peekIndexRaw 1 peekText idx
      classes <- fromMaybe [] <$!> optional (peekIndexRaw 2 peekClasses idx)
      attribs <- fromMaybe [] <$!> optional (peekIndexRaw 3 peekAttributeList idx)
      return $ ident `seq` classes `seq` attribs `seq`
        (ident, classes, attribs)
    else retrieving "HTML-like attributes" $ do
      kvs <- peekKeyValuePairs peekText peekText idx
      let ident = fromMaybe "" $ lookup "id" kvs
      let classes = maybe [] T.words $ lookup "class" kvs
      let attribs = filter ((`notElem` ["id", "class"]) . fst) kvs
      return $ ident `seq` classes `seq` attribs `seq`
        (ident, classes, attribs)

-- | Constructor for 'Attr'.
mkAttr :: LuaError e => DocumentedFunction e
mkAttr = defun "Attr"
  ### (\_ _ _ ->
        ltype (nthBottom 1) >>= \case
          TypeString -> forcePeek $ do
            mident <- optional (peekText (nthBottom 1))
            mclass <- optional (peekList peekText (nthBottom 2))
            mattribs <- optional (peekAttributeList (nthBottom 3))
            return ( fromMaybe "" mident
                   , fromMaybe [] mclass
                   , fromMaybe [] mattribs)
          TypeTable  -> forcePeek $ peekAttrTable (nthBottom 1)
          TypeUserdata -> forcePeek $ peekUD typeAttr (nthBottom 1) <|> do
            attrList <- peekUD typeAttributeList (nthBottom 1)
            return ("", [], attrList)
          TypeNil    -> pure nullAttr
          TypeNone   -> pure nullAttr
          x          -> failLua $ "Cannot create Attr from " ++ show x)
  <#> opt (parameter pure "string|table|Attr" "identifier" "element identifier")
  <#> opt (parameter pure "{string,...}" "classes" "element classes")
  <#> opt (parameter pure "table|AttributeList" "attributes"
           "table containing string keys and values")
  =#> functionResult pushAttr "Attr" "new Attr object"
  #? "Create a new set of attributes"

-- | Constructor for 'AttributeList'.
mkAttributeList :: LuaError e => DocumentedFunction e
mkAttributeList = defun "AttributeList"
  ### return
  <#> parameter peekAttributeList "table|AttributeList" "attribs"
        "an attribute list"
  =#> functionResult (pushUD typeAttributeList) "AttributeList"
        "new AttributeList object"
