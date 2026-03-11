{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{- |

Marshal values of types that make up 'Block' elements.
-}
module Text.Pandoc.Lua.Marshal.Block
  ( -- * Single Block elements
    typeBlock
  , peekBlock
  , peekBlockFuzzy
  , pushBlock
    -- * List of Blocks
  , peekBlocks
  , peekBlocksFuzzy
  , pushBlocks
    -- * Constructors
  , blockConstructors
  , mkBlocks
    -- * Walk
  , walkBlockSplicing
  , walkBlocksStraight
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad.Catch (throwM)
import Control.Monad ((<$!>))
import Data.Aeson (encode)
import Data.Data (showConstr, toConstr)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import HsLua hiding (Div)
import Text.Pandoc.Lua.Marshal.Attr (peekAttr, pushAttr)
import Text.Pandoc.Lua.Marshal.Caption (peekCaptionFuzzy, pushCaption)
import Text.Pandoc.Lua.Marshal.Content
  ( Content (..), contentTypeDescription, peekContent, pushContent
  , peekDefinitionItem )
import Text.Pandoc.Lua.Marshal.Filter (Filter, peekFilter)
import Text.Pandoc.Lua.Marshal.Format (peekFormat, pushFormat)
import Text.Pandoc.Lua.Marshal.Inline (peekInlinesFuzzy)
import Text.Pandoc.Lua.Marshal.List (newListMetatable, pushPandocList)
import Text.Pandoc.Lua.Marshal.ListAttributes
  ( peekListAttributes, pushListAttributes )
import Text.Pandoc.Lua.Marshal.Shared (walkBlocksAndInlines)
import Text.Pandoc.Lua.Marshal.TableParts
  ( peekColSpec, pushColSpec
  , peekTableBodyFuzzy, pushTableBody
  , peekTableFoot, pushTableFoot
  , peekTableHead, pushTableHead
  )
import Text.Pandoc.Lua.Walk (SpliceList, Walkable, walkStraight, walkSplicing)
import Text.Pandoc.Definition

-- | Pushes an Block value as userdata object.
pushBlock :: LuaError e => Pusher e Block
pushBlock = pushUD typeBlock
{-# INLINE pushBlock #-}

-- | Retrieves an Block value.
peekBlock :: LuaError e => Peeker e Block
peekBlock = peekUD typeBlock
{-# INLINE peekBlock #-}

-- | Retrieves a list of Block values.
peekBlocks :: LuaError e
           => Peeker e [Block]
peekBlocks = peekList peekBlock
{-# INLINABLE peekBlocks #-}

-- | Pushes a list of Block values.
pushBlocks :: LuaError e
           => Pusher e [Block]
pushBlocks xs = do
  pushList pushBlock xs
  newListMetatable "Blocks" $ do
    pushName "walk"
    pushDocumentedFunction $ lambda
      ### flip walkBlocksAndInlines
      <#> parameter peekBlocksFuzzy "Blocks" "self" ""
      <#> parameter peekFilter "Filter" "lua_filter" "table of filter functions"
      =#> functionResult pushBlocks "Blocks" "modified list"
    rawset (nth 3)

    pushName "clone"
    pushDocumentedFunction $ lambda
      ### return
      <#> parameter peekBlocksFuzzy "Blocks" "self" ""
      =#> functionResult pushBlocks "Blocks" "deep copy"
    rawset (nth 3)

    pushName "__tostring"
    pushDocumentedFunction $ lambda
      ### liftPure show
      <#> parameter peekBlocksFuzzy "Blocks" "self" ""
      =#> functionResult pushString "string" "native Haskell representation"
    rawset (nth 3)

    pushName "__tojson"
    pushDocumentedFunction $ lambda
      ### liftPure encode
      <#> parameter peekBlocksFuzzy "Blocks" "self" ""
      =#> functionResult pushLazyByteString "string" "JSON representation"
    rawset (nth 3)
  setmetatable (nth 2)
{-# INLINABLE pushBlocks #-}

-- | Unmarshal a table as Block value by calling the @__toblock@ metamethod
-- first.
peekBlockMetamethod :: LuaError e
                    => Peeker e Block
peekBlockMetamethod idx = do
  absidx <- liftLua $ absindex idx
  liftLua (getmetafield absidx "__toblock") >>= \case
    TypeNil      -> failPeek "object has no __toblock metamethod"
    TypeFunction -> do
      liftLua (pushvalue absidx)
      liftLua (pcall 1 1 Nothing) >>= \case
        OK   -> peekBlock top `lastly` pop 1
        _err -> do
          msg <- peekByteString top `lastly` pop 1
          failPeek $ "failure in __toblock: " <> msg
    _otherType   -> do
      liftLua (pop 1)   -- drop "__toblock" field
      failPeek "__toblock metafield does not contain a function"

-- | Try extra hard to retrieve a Block value from the stack. Treats
-- bare strings as @Str@ values.
peekBlockFuzzy :: LuaError e
               => Peeker e Block
peekBlockFuzzy idx =
       peekBlock idx
  <|> peekBlockMetamethod idx
  <|> (Plain <$!> peekInlinesFuzzy idx)
  <|> (failPeek =<<
       typeMismatchMessage "Block or list of Inlines" idx)
{-# INLINABLE peekBlockFuzzy #-}

-- | Try extra-hard to return the value at the given index as a list of
-- 'Block's.
peekBlocksFuzzy :: LuaError e
                => Peeker e [Block]
peekBlocksFuzzy idx =
      ((:[]) <$> peekBlockMetamethod idx)
  <|> peekList peekBlockFuzzy idx
  <|> (pure <$!> peekBlockFuzzy idx)
  <|> (failPeek =<<
       typeMismatchMessage "Block, list of Blocks, or compatible element" idx)
{-# INLINABLE peekBlocksFuzzy #-}

-- | Block object type.
typeBlock :: forall e. LuaError e => DocumentedType e Block
typeBlock = deftype "Block"
  [ operation Eq $ lambda
    ### liftPure2 (\a b -> fromMaybe False ((==) <$> a <*> b))
    <#> parameter (optional . peekBlockFuzzy) "Block" "a" ""
    <#> parameter (optional . peekBlockFuzzy) "Block" "b" ""
    =#> boolResult "whether the two values are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> udparam typeBlock "self" ""
    =#> functionResult pushString "string" "Haskell representation"
  , operation (CustomOperation "__tojson") $ lambda
    ### liftPure encode
    <#> udparam typeBlock "self" ""
    =#> functionResult pushLazyByteString "string" "JSON representation"
  ]
  [ possibleProperty "attr" "element attributes"
      (pushAttr, \case
          CodeBlock attr _     -> Actual attr
          Div attr _           -> Actual attr
          Figure attr _ _      -> Actual attr
          Header _ attr _      -> Actual attr
          Table attr _ _ _ _ _ -> Actual attr
          _                    -> Absent)
      (peekAttr, \case
          CodeBlock _ code     -> Actual . flip CodeBlock code
          Div _ blks           -> Actual . flip Div blks
          Figure _ capt blks   -> Actual . (\attr -> Figure attr capt blks)
          Header lvl _ blks    -> Actual . (\attr -> Header lvl attr blks)
          Table _ c cs h bs f  -> Actual . (\attr -> Table attr c cs h bs f)
          _                    -> const Absent)
  , possibleProperty "bodies" "table bodies"
      (pushPandocList pushTableBody, \case
          Table _ _ _ _ bs _ -> Actual bs
          _                  -> Absent)
      (peekList peekTableBodyFuzzy, \case
          Table attr c cs h _ f -> Actual . (\bs -> Table attr c cs h bs f)
          _                     -> const Absent)
  , possibleProperty "caption" "element caption"
      (pushCaption, \case
          Figure _ capt _      -> Actual capt
          Table _ capt _ _ _ _ -> Actual capt
          _ -> Absent)
      (peekCaptionFuzzy, \case
          Figure attr _ blks     -> Actual . (\c -> Figure attr c blks)
          Table attr _ cs h bs f -> Actual . (\c -> Table attr c cs h bs f)
          _                      -> const Absent)
  , possibleProperty "colspecs" "column alignments and widths"
      (pushPandocList pushColSpec, \case
          Table _ _ cs _ _ _     -> Actual cs
          _                      -> Absent)
      (peekList peekColSpec, \case
          Table attr c _ h bs f  -> Actual . (\cs -> Table attr c cs h bs f)
          _                      -> const Absent)
  , possibleProperty "content" "element content"
      (pushContent, getBlockContent)
      (peekContent, setBlockContent (Proxy @e))
  , possibleProperty "foot" "table foot"
      (pushTableFoot, \case {Table _ _ _ _ _ f -> Actual f; _ -> Absent})
      (peekTableFoot, \case
          Table attr c cs h bs _ -> Actual . Table attr c cs h bs
          _                      -> const Absent)
  , possibleProperty "format" "format of raw content"
      (pushFormat, \case {RawBlock f _ -> Actual f; _ -> Absent})
      (peekFormat, \case
          RawBlock _ txt -> Actual . (`RawBlock` txt)
          _              -> const Absent)
  , possibleProperty "head" "table head"
      (pushTableHead, \case {Table _ _ _ h _ _ -> Actual h; _ -> Absent})
      (peekTableHead, \case
          Table attr c cs _ bs f  -> Actual . (\h -> Table attr c cs h bs f)
          _                       -> const Absent)
  , possibleProperty "level" "heading level"
      (pushIntegral, \case {Header lvl _ _ -> Actual lvl; _ -> Absent})
      (peekIntegral, \case
          Header _ attr inlns -> Actual . \lvl -> Header lvl attr inlns
          _                   -> const Absent)
  , possibleProperty "listAttributes" "ordered list attributes"
      (pushListAttributes, \case
          OrderedList listAttr _ -> Actual listAttr
          _                      -> Absent)
      (peekListAttributes, \case
          OrderedList _ content -> Actual . (`OrderedList` content)
          _                     -> const Absent)
  , possibleProperty "text" "text contents"
      (pushText, getBlockText)
      (peekText, setBlockText)

  , readonly "tag" "type of Block"
      (pushString, showConstr . toConstr )

  , alias "t" "tag" ["tag"]
  , alias "c" "content" ["content"]
  , alias "identifier" "element identifier"       ["attr", "identifier"]
  , alias "classes"    "element classes"          ["attr", "classes"]
  , alias "attributes" "other element attributes" ["attr", "attributes"]
  , alias "start"      "ordered list start number" ["listAttributes", "start"]
  , alias "style"      "ordered list style"       ["listAttributes", "style"]
  , alias "delimiter"  "numbering delimiter"      ["listAttributes", "delimiter"]

  , method $ defun "clone"
    ### return
    <#> parameter peekBlock "Block" "block" "self"
    =#> functionResult pushBlock "Block" "cloned Block"

  , method $ defun "show"
    ### liftPure show
    <#> parameter peekBlock "Block" "self" ""
    =#> functionResult pushString "string" "Haskell string representation"

  , method $ defun "walk"
    ### flip walkBlocksAndInlines
    <#> parameter peekBlock "Block" "self" ""
    <#> parameter peekFilter "Filter" "lua_filter" "table of filter functions"
    =#> functionResult pushBlock "Block" "modified element"
  ]

getBlockContent :: Block -> Possible Content
getBlockContent = \case
  -- inline content
  Para inlns          -> Actual $ ContentInlines inlns
  Plain inlns         -> Actual $ ContentInlines inlns
  Header _ _ inlns    -> Actual $ ContentInlines inlns
  -- block content
  BlockQuote blks     -> Actual $ ContentBlocks blks
  Div _ blks          -> Actual $ ContentBlocks blks
  Figure _ _ blks     -> Actual $ ContentBlocks blks
  -- lines content
  LineBlock lns       -> Actual $ ContentLines lns
  -- list items content
  BulletList itms     -> Actual $ ContentListItems itms
  OrderedList _ itms  -> Actual $ ContentListItems itms
  -- definition items content
  DefinitionList itms -> Actual $ ContentDefItems itms
  _                   -> Absent

setBlockContent :: forall e. LuaError e
                => Proxy e -> Block -> Content -> Possible Block
setBlockContent _ = \case
  -- inline content
  Para _           -> Actual . Para . inlineContent
  Plain _          -> Actual . Plain . inlineContent
  Header attr lvl _ -> Actual . Header attr lvl . inlineContent
  -- block content
  BlockQuote _     -> Actual . BlockQuote . blockContent
  Div attr _       -> Actual . Div attr . blockContent
  Figure attr c _  -> Actual . Figure attr c . blockContent
  -- lines content
  LineBlock _      -> Actual . LineBlock . lineContent
  -- list items content
  BulletList _     -> Actual . BulletList . listItemContent
  OrderedList la _ -> Actual . OrderedList la . listItemContent
  -- definition items content
  DefinitionList _ -> Actual . DefinitionList . defItemContent
  _                -> const Absent
 where
    inlineContent = \case
      ContentInlines inlns -> inlns
      c -> throwM . luaException @e $
           "expected Inlines, got " <> contentTypeDescription c
    blockContent = \case
      ContentBlocks blks   -> blks
      ContentInlines []    -> []
      ContentInlines inlns -> [Plain inlns]
      c -> throwM . luaException @e $
           "expected Blocks, got " <> contentTypeDescription c
    lineContent = \case
      ContentLines lns     -> lns
      c -> throwM . luaException @e $
           "expected list of lines (Inlines), got " <> contentTypeDescription c
    defItemContent = \case
      ContentDefItems itms -> itms
      c -> throwM . luaException @e $
           "expected definition items, got " <> contentTypeDescription c
    listItemContent = \case
      ContentBlocks blks    -> map (:[]) blks
      ContentLines lns      -> map ((:[]) . Plain) lns
      ContentListItems itms -> itms
      c -> throwM . luaException @e $
           "expected list of items, got " <> contentTypeDescription c

getBlockText :: Block -> Possible Text
getBlockText = \case
  CodeBlock _ lst -> Actual lst
  RawBlock _ raw  -> Actual raw
  _               -> Absent

setBlockText :: Block -> Text -> Possible Block
setBlockText = \case
  CodeBlock attr _ -> Actual . CodeBlock attr
  RawBlock f _     -> Actual . RawBlock f
  _                -> const Absent

-- | Constructor functions for 'Block' elements.
blockConstructors :: LuaError e => [DocumentedFunction e]
blockConstructors =
  [ defun "BlockQuote"
    ### liftPure BlockQuote
    <#> blocksParam
    =#> blockResult "BlockQuote element"
    #? "Creates a block quote element"

  , defun "BulletList"
    ### liftPure BulletList
    <#> blockItemsParam "list items"
    =#> blockResult "BulletList element"
    #? "Creates a bullet list."

  , defun "CodeBlock"
    ### liftPure2 (\code mattr -> CodeBlock (fromMaybe nullAttr mattr) code)
    <#> textParam "text" "code string"
    <#> optAttrParam
    =#> blockResult "CodeBlock element"
    #? "Creates a code block element."

  , defun "DefinitionList"
    ### liftPure DefinitionList
    <#> parameter (choice
                   [ peekList peekDefinitionItem
                   , \idx -> (:[]) <$!> peekDefinitionItem idx
                   ])
                  "{{Inlines, {Blocks,...}},...}"
                  "content" "definition items"
    =#> blockResult "DefinitionList element"
    #? "Creates a definition list, containing terms and their explanation."

  , defun "Div"
    ### liftPure2 (\content mattr -> Div (fromMaybe nullAttr mattr) content)
    <#> blocksParam
    <#> optAttrParam
    =#> blockResult "Div element"
    #? "Creates a div element"

  , defun "Figure"
    ### liftPure3 (\content mcapt mattr ->
                     let attr = fromMaybe nullAttr mattr
                         capt = fromMaybe (Caption mempty mempty) mcapt
                     in Figure attr capt content)
    <#> parameter peekBlocksFuzzy "Blocks" "content" "figure block content"
    <#> opt (parameter peekCaptionFuzzy "Caption" "caption" "figure caption")
    <#> optAttrParam
    =#> blockResult "Figure object"
    #? "Creates a [[Figure]] element."

  , defun "Header"
    ### liftPure3 (\lvl content mattr ->
                     Header lvl (fromMaybe nullAttr mattr) content)
    <#> parameter peekIntegral "integer" "level" "heading level"
    <#> parameter peekInlinesFuzzy "Inlines" "content" "inline content"
    <#> optAttrParam
    =#> blockResult "Header element"
    #? "Creates a header element."

  , defun "HorizontalRule"
    ### return HorizontalRule
    =#> blockResult "HorizontalRule element"
    #? "Creates a horizontal rule."

  , defun "LineBlock"
    ### liftPure LineBlock
    <#> parameter (peekList peekInlinesFuzzy) "{Inlines,...}" "content" "lines"
    =#> blockResult "LineBlock element"
    #? "Creates a line block element."

  , defun "OrderedList"
    ### liftPure2 (\items mListAttrib ->
                     let defListAttrib = (1, DefaultStyle, DefaultDelim)
                     in OrderedList (fromMaybe defListAttrib mListAttrib) items)
    <#> blockItemsParam "list items"
    <#> opt (parameter peekListAttributes "ListAttributes" "listAttributes"
                       "list parameters")
    =#> blockResult "OrderedList element"
    #? "Creates an ordered list."

  , defun "Para"
    ### liftPure Para
    <#> parameter peekInlinesFuzzy "Inlines" "content" "inline content"
    =#> blockResult "Para element"
    #? "Creates a para element."

  , defun "Plain"
    ### liftPure Plain
    <#> parameter peekInlinesFuzzy "Inlines" "content" "inline content"
    =#> blockResult "Plain element"
    #? "Creates a plain element."

  , defun "RawBlock"
    ### liftPure2 RawBlock
    <#> parameter peekFormat "string" "format" "format of content"
    <#> textParam "text" "raw content"
    =#> blockResult "RawBlock element"
    #? "Creates a raw content block of the specified format."

  , defun "Table"
    ### (\capt colspecs thead tbodies tfoot mattr ->
           let attr = fromMaybe nullAttr mattr
           in return $! attr `seq` capt `seq` colspecs `seq` thead `seq` tbodies
              `seq` tfoot `seq` Table attr capt colspecs thead tbodies tfoot)
    <#> parameter peekCaptionFuzzy "Caption" "caption" "table caption"
    <#> parameter (peekList peekColSpec) "{ColSpec,...}" "colspecs"
                  "column alignments and widths"
    <#> parameter peekTableHead "TableHead" "head" "table head"
    <#> parameter (peekList peekTableBodyFuzzy) "{TableBody,...}" "bodies"
                  "table bodies"
    <#> parameter peekTableFoot "TableFoot" "foot" "table foot"
    <#> optAttrParam
    =#> blockResult "Table element"
    #? "Creates a table element."
  ]
 where
  blockResult = functionResult pushBlock "Block"
  blocksParam = parameter peekBlocksFuzzy "Blocks" "content" "block content"
  blockItemsParam = parameter peekItemsFuzzy "{Blocks,...}" "items"
  peekItemsFuzzy idx = peekList peekBlocksFuzzy idx
    <|> ((:[]) <$!> peekBlocksFuzzy idx)

  optAttrParam = opt (parameter peekAttr "Attr" "attr" "element attributes")


-- | Constructor for a list of `Block` values.
mkBlocks :: LuaError e => DocumentedFunction e
mkBlocks = defun "Blocks"
  ### liftPure id
  <#> parameter peekBlocksFuzzy "Blocks" "block_like_elements"
      ("List where each element can be treated as a [[Block]] value, " <>
       "or a single such value.")
  =#> functionResult pushBlocks "Blocks" "list of block elements"
  #? "Creates a [[Blocks]] list."

--
-- walk
--

walkBlockSplicing :: (LuaError e, Walkable (SpliceList Block) a)
                  => Filter -> a -> LuaE e a
walkBlockSplicing = walkSplicing pushBlock peekBlocksFuzzy

walkBlocksStraight :: (LuaError e, Walkable [Block] a)
                   => Filter -> a -> LuaE e a
walkBlocksStraight = walkStraight "Blocks" pushBlocks peekBlocksFuzzy
