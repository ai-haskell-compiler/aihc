{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The HTML parser.
module Zenacy.HTML.Internal.Parser
  ( Parser(..)
  , ParserOptions(..)
  , ParserResult(..)
  , parseDocument
  , parseFragment
  ) where

import Zenacy.HTML.Internal.BS
import Zenacy.HTML.Internal.Buffer
import Zenacy.HTML.Internal.Char
import Zenacy.HTML.Internal.Core
import Zenacy.HTML.Internal.DOM
import Zenacy.HTML.Internal.Lexer
import Zenacy.HTML.Internal.Token
import Zenacy.HTML.Internal.Types

import Control.Applicative
  ( liftA
  )
import Control.Monad
  ( when
  , unless
  , void
  )
import Control.Monad.Extra
  ( (||^)
  , (&&^)
  , anyM
  , notM
  , whenM
  , whenJustM
  , unlessM
  )
import Control.Monad.ST
  ( ST
  , runST
  )
import Data.Default
  ( Default(..)
  )
import Data.DList
  ( DList
  )
import qualified Data.DList as D
  ( append
  , empty
  , snoc
  , toList
  )
import Data.IntMap
  ( IntMap
  )
import qualified Data.IntMap as IntMap
  ( findWithDefault
  , lookup
  , insert
  , map
  , mapWithKey
  )
import Data.List
  ( find
  )
import Data.Map
  ( Map
  )
import qualified Data.Map as Map
  ( fromList
  , lookup
  )
import Data.Maybe
  ( fromJust
  , isJust
  , isNothing
  , listToMaybe
  , mapMaybe
  )
import Data.Monoid
  ( (<>)
  )
import Data.Sequence
  ( Seq
  )
import qualified Data.Sequence as Seq
  ( empty
  , fromList
  )
import Data.Set
  ( Set
  )
import qualified Data.Set as Set
  ( fromList
  , member
  , notMember
  , union
  , unions
  )
import Data.STRef
  ( STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  )
import Data.Word
  ( Word8
  )

-- | Parser processing state.
data Parser s = Parser
  { parserLexer             :: STRef s (Lexer s)
  -- ^ The lexer for token generation.
  , parserDOM               :: STRef s DOM
  -- ^ The parser DOM.
  , parserElementStack      :: STRef s [DOMID]
  -- ^ The element stack (section 12.2.3.2).
  , parserActiveFormatList  :: STRef s [ParserFormatItem]
  -- ^ The list of action formatting elements (section 12.2.3.3).
  , parserInsertionMode     :: STRef s ParserMode
  -- ^ The current insertion mode.
  , parserOriginalMode      :: STRef s ParserMode
  -- ^ The original insertion mode.
  , parserTemplateMode      :: STRef s [ParserMode]
  -- ^ The template insertion mode.
  , parserContextElement    :: STRef s (Maybe DOMID)
  -- ^ The context element.
  , parserHeadElement       :: STRef s (Maybe DOMID)
  -- ^ The head element pointer (section 12.2.3.4).
  , parserFormElement       :: STRef s (Maybe DOMID)
  -- ^ The form element pointer (section 12.2.3.4).
  , parserSelfClosingFlag   :: STRef s Bool
  -- ^ The self closing acknowledges flag.
  , parserFragmentMode      :: STRef s Bool
  -- ^ The flag indicating parser is in fragment mode.
  , parserFosterParenting   :: STRef s Bool
  -- ^ The foster parenting flag.
  , parserFrameSetOK        :: STRef s Bool
  -- ^ The frame-set ok flag (section 12.2.3.5).
  , parserDone              :: STRef s Bool
  -- ^ The parser done flag.
  , parserTableChars        :: STRef s [Token]
  -- ^ The pending table characters.
  , parserAdoptionAgency    :: STRef s (ParserAdoptionAgency s)
  -- ^ The adoption agency state.
  , parserErrors            :: STRef s (DList BS)
  -- ^ The parser errors.
  , parserIFrameSrcDoc      :: STRef s Bool
  -- ^ Indicates that the documnet is an iframe srcdoc.
  , parserTextMap           :: STRef s (IntMap (STRef s (Buffer s)))
  -- ^ Map of buffers for holding dom strings.
  , parserLogErrors         :: Bool
  -- ^ Flag to log errors.
  }

-- | Defines the parser mode.
data ParserMode
  = ModeInitial
  | ModeBeforeHtml
  | ModeBeforeHead
  | ModeInHead
  | ModeInHeadNoscript
  | ModeAfterHead
  | ModeInBody
  | ModeText
  | ModeInTable
  | ModeInTableText
  | ModeInCaption
  | ModeInColumnGroup
  | ModeInTableBody
  | ModeInRow
  | ModeInCell
  | ModeInSelect
  | ModeInSelectInTable
  | ModeInTemplate
  | ModeAfterBody
  | ModeInFrameset
  | ModeAfterFrameset
  | ModeAfterAfterBody
  | ModeAfterAfterFrameset
    deriving (Eq, Ord, Show)

-- | Parser options type.
data ParserOptions = ParserOptions
  { parserOptionInput          :: BS
  -- ^ The input to the lexer.
  , parserOptionLogErrors      :: Bool
  -- ^ Indicates whether warnings are logged.
  , parserOptionIgnoreEntities :: Bool
  -- ^ Indicates that entities should not be tokenized.
  } deriving (Eq, Ord, Show)

-- | Parser result type.
data ParserResult = ParserResult
  { parserResultDOM    :: DOM
  , parserResultErrors :: [BS]
  } deriving (Eq, Ord, Show)

-- | Defines an item in the list of active formatting elements.
data ParserFormatItem
  = ParserFormatElement DOMID Token
  | ParserFormatMarker
    deriving (Eq, Ord, Show)

-- | Defines element categories.
data ParserElementCategory
  = ElementCategorySpecial
  | ElementCategoryFormatting
  | ElementCategoryOrdinary
    deriving (Eq, Ord, Show)

-- | Defines detailed information for stack elements.
data ElementDetails = ElementDetails
  { elementDetailsIndex :: Int
  , elementDetailsID    :: DOMID
  , elementDetailsNode  :: DOMNode
  , elementDetailsType  :: DOMType
  } deriving (Eq, Ord, Show)

-- | Default instance for parser options.
instance Default ParserOptions where
  def = ParserOptions
    { parserOptionInput          = bsEmpty
    , parserOptionLogErrors      = False
    , parserOptionIgnoreEntities = False
    }

-- | Default instance for parser results.
instance Default ParserResult where
  def = ParserResult
    { parserResultDOM    = def
    , parserResultErrors = []
    }

-- | Parses an HTML document.
parseDocument :: ParserOptions -> Either BS ParserResult
parseDocument x =
  runST $ do
    parserNew x >>= \case
      Right p -> Right <$> parserRun p
      Left e -> Left <$> pure e

-- | Parses an HTML fragment.
parseFragment :: ParserOptions -> Either BS ParserResult
parseFragment x = Left "fragment support not yet implemented"

-- | Makes a new lexer.
parserNew :: ParserOptions -> ST s (Either BS (Parser s))
parserNew o@ParserOptions{..} = do
  a <- lexerNew def
    { lexerOptionInput          = parserOptionInput
    , lexerOptionLogErrors      = parserOptionLogErrors
    , lexerOptionIgnoreEntities = parserOptionIgnoreEntities
    }
  case a of
    Right lex -> Right <$> parserMake o lex
    Left err  -> Left <$> pure err

-- | Makes a new lexer.
parserMake :: ParserOptions -> Lexer s -> ST s (Parser s)
parserMake ParserOptions{..} lexer = do
  lexerRef <- newSTRef lexer
  dom      <- newSTRef def
  stack    <- newSTRef []
  fmtList  <- newSTRef []
  insMode  <- newSTRef ModeInitial
  orgMode  <- newSTRef ModeInitial
  tmpMode  <- newSTRef []
  ctxElem  <- newSTRef Nothing
  headElem <- newSTRef Nothing
  formElem <- newSTRef Nothing
  closing  <- newSTRef False
  fragMode <- newSTRef False
  foster   <- newSTRef False
  frameSet <- newSTRef True
  done     <- newSTRef False
  table    <- newSTRef []
  aa       <- defaultAA
  aaRef    <- newSTRef aa
  warn     <- newSTRef D.empty
  iframe   <- newSTRef False
  textMap  <- newSTRef def
  pure $ Parser
    { parserLexer             = lexerRef
    , parserDOM               = dom
    , parserElementStack      = stack
    , parserActiveFormatList  = fmtList
    , parserInsertionMode     = insMode
    , parserOriginalMode      = orgMode
    , parserTemplateMode      = tmpMode
    , parserContextElement    = ctxElem
    , parserHeadElement       = headElem
    , parserFormElement       = formElem
    , parserSelfClosingFlag   = closing
    , parserFragmentMode      = fragMode
    , parserFosterParenting   = foster
    , parserFrameSetOK        = frameSet
    , parserDone              = done
    , parserTableChars        = table
    , parserAdoptionAgency    = aaRef
    , parserErrors            = warn
    , parserIFrameSrcDoc      = iframe
    , parserTextMap           = textMap
    , parserLogErrors         = parserOptionLogErrors
    }

-- | The parser main loop.
parserRun :: Parser s -> ST s ParserResult
parserRun p@Parser {..} = do
  rref parserDone >>= \case
    True -> do
      Lexer{..} <- rref parserLexer
      e <- D.append <$> rref lexerErrors <*> rref parserErrors
      d <- textMapDOM p
      pure $ ParserResult d $ D.toList e
    False -> do
      t <- rref parserLexer >>= lexerNext
      selfClosingInit p t
      dispatchTreeConstruction p t
      whenM (selfClosingFlag p) $
        parseError p (Just t) "self closing not acknowledged for token"
      parserRun p

-- | Handles tree construction dispatch.
dispatchTreeConstruction :: Parser s -> Token -> ST s ()
dispatchTreeConstruction p@Parser {..} t = do
  e <- elementStackEmpty p
  a <- adjustedCurrentNode p
  b <- pure $ case a of
    Just n -> domNodeIsHTML n
      || isMathMLIntegrationPoint n
         && isTokenStartNotNamed t ["mglyph", "malignmark"]
      || isMathMLIntegrationPoint n && tokenIsChar t
      || isMathMLElementNamed n "annotation-xml"
         && isTokenStartNamed t ["svg"]
      || isHtmlIntgrationPoint n && tokenIsStart t
      || isHtmlIntgrationPoint n && tokenIsChar t
    Nothing -> False
  if e || b || tokenIsEOF t
     then doHtmlContent p t
     else doForeignContent p t
  where
    tokenIsChar TChar {} = True
    tokenIsChar _ = False
    tokenIsStart TStart {} = True
    tokenIsStart _ = False
    tokenIsEOF TEOF = True
    tokenIsEOF _ = False

-- | Processes a token as HTML content.
doHtmlContent :: Parser s -> Token -> ST s ()
doHtmlContent p@Parser {..} t = do
  m <- rref parserInsertionMode
  parserInserter m p t

-- | Reprocesses a token.
reprocess :: Parser s -> Token -> ST s ()
reprocess = doHtmlContent

-- | Gets the inserter for a parser mode.
parserInserter :: ParserMode -> Parser s -> Token -> ST s ()
parserInserter = \case
  ModeInitial            -> doModeInitial
  ModeBeforeHtml         -> doModeBeforeHtml
  ModeBeforeHead         -> doModeBeforeHead
  ModeInHead             -> doModeInHead
  ModeInHeadNoscript     -> doModeInHeadNoscript
  ModeAfterHead          -> doModeAfterHead
  ModeInBody             -> doModeInBody
  ModeText               -> doModeText
  ModeInTable            -> doModeInTable
  ModeInTableText        -> doModeInTableText
  ModeInCaption          -> doModeInCaption
  ModeInColumnGroup      -> doModeInColumnGroup
  ModeInTableBody        -> doModeInTableBody
  ModeInRow              -> doModeInRow
  ModeInCell             -> doModeInCell
  ModeInSelect           -> doModeInSelect
  ModeInSelectInTable    -> doModeInSelectInTable
  ModeInTemplate         -> doModeInTemplate
  ModeAfterBody          -> doModeAfterBody
  ModeInFrameset         -> doModeInFrameset
  ModeAfterFrameset      -> doModeAfterFrameset
  ModeAfterAfterBody     -> doModeAfterAfterBody
  ModeAfterAfterFrameset -> doModeAfterAfterFrameset

-- | Handles parse errors.
parseError :: Parser s -> Maybe Token -> BS -> ST s ()
parseError p@Parser {..} t s =
  when parserLogErrors $
    uref parserErrors $ flip D.snoc e
  where
    e = s <> case t of
      Just (TDoctype {..}) -> ",doctype"
      Just (TStart {..})   -> ",tag-start," <> tStartName
      Just (TEnd {..})     -> ",tag-end," <> tEndName
      Just (TComment {..}) -> ",comment"
      Just (TChar {..})    -> ",chr," <> bsOnly tCharData
      Just TEOF           -> ",eof"
      Nothing             -> bsEmpty

-- | Detmermines if a token is a start token with a specified name.
isTokenStartNamed :: Token -> [BS] -> Bool
isTokenStartNamed TStart {..} names = tStartName `elem` names
isTokenStartNamed _ _ = False

-- | Detmermines if a token is a start token without a specified name.
isTokenStartNotNamed :: Token -> [BS] -> Bool
isTokenStartNotNamed TStart {..} names = not $ tStartName `elem` names
isTokenStartNotNamed _ _ = False

-- | Detmermines if a token is an end token with a specified name.
isTokenEndNamed :: Token -> [BS] -> Bool
isTokenEndNamed TEnd {..} names = tEndName `elem` names
isTokenEndNamed _ _ = False

-- | Detmermines if a token is an end token without a specified name.
isTokenEndNotNamed :: Token -> [BS] -> Bool
isTokenEndNotNamed TEnd {..} names = not $ tEndName `elem` names
isTokenEndNotNamed _ _ = False

-- | Determines if a node name matches a name.
elementName :: BS -> DOMNode -> Bool
elementName x y = domNodeElementName y == x

-- | Determines if a node name does not match a name.
elementNameNot :: BS -> DOMNode -> Bool
elementNameNot x = not . elementName x

-- | Determines if a node name is in a set of names.
elementNameIn :: [BS] -> DOMNode -> Bool
elementNameIn x y = domNodeElementName y `elem` x

-- | Determines if a node name is not in a set of names.
elementNameNotIn :: [BS] -> DOMNode -> Bool
elementNameNotIn x = not . elementNameIn x

-- | Gets the current element stack.
elementStack :: Parser s -> ST s [DOMID]
elementStack Parser {..} = readSTRef parserElementStack

-- | Detmermines if the element stack is empty.
elementStackEmpty :: Parser s -> ST s Bool
elementStackEmpty p@Parser {..} = null <$> elementStack p

-- | Returns the size of the element stack.
elementStackSize :: Parser s -> ST s Int
elementStackSize p@Parser {..} = length <$> elementStack p

-- | Modifies the element stack by applying a function.
elementStackModify :: Parser s -> ([DOMID] -> [DOMID]) -> ST s ()
elementStackModify p@Parser {..} f = uref parserElementStack f

-- | Pushes an element on the element stack.
elementStackPush :: Parser s -> DOMID -> ST s ()
elementStackPush p@Parser {..} x = elementStackModify p $ (x:)

-- | Pops an element off of the element stack.
elementStackPop :: Parser s -> ST s ()
elementStackPop p@Parser {..} = elementStackModify p $ drop 1

-- | Pops nodes from the element stack while a predicate is true.
elementStackPopWhile :: Parser s -> (DOMNode -> Bool) -> ST s ()
elementStackPopWhile p@Parser {..} f =
  currentNode p >>= \case
    Just a | f a -> elementStackPop p >> elementStackPopWhile p f
    _ -> pure ()

-- | Pops a nodes from the element stack if a predicate is true.
elementStackPopIf :: Parser s -> (DOMNode -> Bool) -> ST s ()
elementStackPopIf p@Parser {..} f =
  currentNode p >>= \case
    Just a | f a -> elementStackPop p
    _ -> pure ()

-- | Pops elements from the stack until a specified element has been popped.
elementStackPopUntil :: Parser s -> (DOMType -> Bool) -> ST s ()
elementStackPopUntil p@Parser {..} f = do
  elementStackPopWhile p (not . g)
  elementStackPopIf p g
  where
    g = f . domNodeType

-- | Pops elements from the stack until a specified ID has been popped.
elementStackPopUntilID :: Parser s -> DOMID -> ST s ()
elementStackPopUntilID p x = elementStackModify p $ drop 1 . dropWhile (/=x)

-- | Pops elements from the stack until a specified element has been popped.
elementStackPopUntilType :: Parser s -> DOMType -> ST s ()
elementStackPopUntilType p x = elementStackPopUntil p (==x)

-- | Pops elements from the stack until a specified element has been popped.
elementStackPopUntilTypeIn :: Parser s -> [DOMType] -> ST s ()
elementStackPopUntilTypeIn p x = elementStackPopUntil p $ flip elem x

-- | Gets the current element stack as a list of nodes.
elementStackNodes :: Parser s -> ST s [DOMNode]
elementStackNodes p = domMapID <$> getDOM p <*> elementStack p

-- | Gets the current element stack as a list of types.
elementStackTypes :: Parser s -> ST s [DOMType]
elementStackTypes p = map domNodeType <$> elementStackNodes p

-- | Applies a predicate to the element stack and returns whether
-- any element in the stack results in a true predicate.
elementStackAny :: Parser s -> (DOMNode -> Bool) -> ST s Bool
elementStackAny p f = any f <$> elementStackNodes p

-- | Applies a predicate to the element stack and returns whether
-- all elements in the stack result in a true predicate.
elementStackAll :: Parser s -> (DOMNode -> Bool) -> ST s Bool
elementStackAll p f = all f <$> elementStackNodes p

-- | Determines if the second element on the stack is a body element.
elementStackHasBody :: Parser s -> ST s Bool
elementStackHasBody p =
  liftA reverse (elementStackTypes p) >>= pure . \case
    (_:x:_)    -> x == domMakeTypeHTML "body"
    _otherwise -> False

-- | Determines if the element stack has a template element.
elementStackHasTemplate :: Parser s -> ST s Bool
elementStackHasTemplate p = elementStackAny p domNodeIsTemplate

-- | Determines if the element stack does not have any template elements.
elementStackMissingTemplate :: Parser s -> ST s Bool
elementStackMissingTemplate p = elementStackAll p $ not . domNodeIsTemplate

-- | Removes a node ID from the element stack.
elementStackRemove :: Parser s -> DOMID -> ST s ()
elementStackRemove p x = elementStackModify p $ filter (/=x)

-- | Replaces an ID in the element stack with another ID.
elementStackReplace :: Parser s -> DOMID -> DOMID -> ST s ()
elementStackReplace p x y =
  elementStackModify p $ map (\i -> if i == x then y else i)

-- | Finds the successor for an entry in the element stack.
elementStackSucc :: Parser s -> DOMID -> ST s (Maybe DOMID)
elementStackSucc p x = findSucc (==x) <$> elementStack p

-- | Inserts a node before another node in the element stack.
elementStackInsertBefore :: Parser s -> DOMID -> DOMID -> ST s ()
elementStackInsertBefore p x y = elementStackModify p $ insertBefore (==x) y

-- | Gets the element stack details.
elementStackDetails :: Parser s -> ST s [ElementDetails]
elementStackDetails p = g <$> getDOM p <*> elementStack p
  where
    g d x = mapMaybe (f d) $ zip [1..] x
    f d (i, x) =
      case domGetNode d x of
        Nothing -> Nothing
        Just a -> Just $ ElementDetails i x a $ domNodeType a

-- | Finds element stack details.
elementStackFind :: Parser s -> (ElementDetails -> Bool) -> ST s (Maybe ElementDetails)
elementStackFind p f = liftA (find f) $ elementStackDetails p

-- | Special element types.
elementTypesSpecial :: Set DOMType
elementTypesSpecial = Set.unions
  [ Set.fromList $ domTypesHTML
    [ "address", "applet", "area", "article", "aside",
      "base", "basefont", "bgsound", "blockquote", "body",
      "br", "button", "caption", "center", "col", "colgroup",
      "dd", "details", "dir", "div", "dl", "dt", "embed",
      "fieldset", "figcaption", "figure", "footer", "form",
      "frame", "frameset", "h1", "h2", "h3", "h4", "h5", "h6",
      "head", "header", "hgroup", "hr", "html", "iframe",
      "img", "input", "isindex", "li", "link", "listing",
      "main", "marquee", "menu", "menuitem", "meta", "nav",
      "noembed", "noframes", "noscript", "object", "ol",
      "p", "param", "plaintext", "pre", "script", "section",
      "select", "source", "style", "summary", "table",
      "tbody", "td", "template", "textarea", "tfoot",
      "th", "thead", "title", "tr", "track", "ul", "wbr" ]
  , Set.fromList $ domTypesMathML
    [ "mi", "mo", "mn", "ms", "mtext", "annotation-xml" ]
  , Set.fromList $ domTypesSVG
    [ "foreignObject", "desc", "title" ]
  ]

-- | Formatting element types.
elementTypesFormatting :: Set DOMType
elementTypesFormatting =
  Set.fromList $ domTypesHTML
  [ "a", "b", "big", "code", "em", "font", "i", "nobr",
    "s", "small", "strike", "strong", "tt", "u"]

-- | Returns the element category for an element type.
elementCategory :: DOMType -> ParserElementCategory
elementCategory x
  | Set.member x elementTypesSpecial = ElementCategorySpecial
  | Set.member x elementTypesFormatting = ElementCategoryFormatting
  | otherwise = ElementCategoryOrdinary

-- | Detmermines if an element is in the special category.
elementIsSpecial :: DOMType -> Bool
elementIsSpecial x = elementCategory x == ElementCategorySpecial

-- | Determines if an element is in specific scope.
-- Algorithm from section 12.2.3.2 is specification.
elementInSpecificScope :: Parser s -> Bool -> Set DOMType -> DOMType -> ST s Bool
elementInSpecificScope p include types target =
  f <$> elementStackTypes p
  where
    f :: [DOMType] -> Bool
    f [] = False
    f (x:xs)
      | x == target = True
      | include == True && Set.member x types == True = False
      | include == False && Set.member x types == False = False
      | otherwise = f xs

-- | Default element scopes.
elementScopes :: Set DOMType
elementScopes = Set.unions
  [ Set.fromList $ domTypesHTML
    [ "applet", "caption", "html", "table", "td", "th"
    , "marquee", "object", "template" ]
  , Set.fromList $ domTypesMathML
    [ "mi", "mo", "mn", "ms", "mtext", "annotation-xml" ]
  , Set.fromList $ domTypesSVG
    [ "foreignObject", "desc", "title" ]
  ]

-- | Determines if an element is in scope.
elementInScope :: Parser s -> DOMType -> ST s Bool
elementInScope p = elementInSpecificScope p True elementScopes

-- | Determines if an element is in list scope.
elementInListScope :: Parser s -> DOMType -> ST s Bool
elementInListScope p =
  elementInSpecificScope p True $
    Set.union elementScopes $ Set.fromList $
      domTypesHTML [ "ol", "ul" ]

-- | Determines if an element is in button scope.
elementInButtonScope :: Parser s -> DOMType -> ST s Bool
elementInButtonScope p =
  elementInSpecificScope p True $
    Set.union elementScopes $ Set.fromList [ domMakeTypeHTML "button" ]

-- | Determines if an element is in table scope.
elementInTableScope :: Parser s -> DOMType -> ST s Bool
elementInTableScope p =
  elementInSpecificScope p True $
    Set.fromList $ domTypesHTML [ "html", "table", "template" ]

-- | Determines if an element is in select scope.
elementInSelectScope :: Parser s -> DOMType -> ST s Bool
elementInSelectScope p =
  elementInSpecificScope p False $
    Set.fromList $ domTypesHTML [ "optgroup", "option" ]

-- | Creates a new ID for a node.
newID :: Parser s -> DOMNode -> ST s DOMID
newID p x = do
  (d, i) <- flip domNewID x <$> getDOM p
  setDOM p d
  pure i

-- | Converts a node ID to a node.
getNode :: Parser s -> DOMID -> ST s (Maybe DOMNode)
getNode p@Parser {..} x = flip domGetNode x <$> getDOM p

-- -- | Gets the element name for a node ID.
nodeElementName :: Parser s -> DOMID -> ST s BS
nodeElementName p@Parser {..} x = do
  d <- getDOM p
  pure $ case domGetNode d x of
    Just a -> domNodeElementName a
    Nothing -> bsEmpty

-- | Gets the last node ID.
lastNodeID :: Parser s -> ST s (Maybe DOMID)
lastNodeID p@Parser {..} = listToMaybe . reverse <$> elementStack p

-- | Gets the current node ID.
currentNodeID :: Parser s -> ST s (Maybe DOMID)
currentNodeID p@Parser {..} = listToMaybe <$> elementStack p

-- | Gets the current node.
currentNode :: Parser s -> ST s (Maybe DOMNode)
currentNode p = currentNodeID p >>= maybe (pure Nothing) (getNode p)

-- | Determines if the current node has a specified type.
currentNodeHasType :: Parser s -> DOMType -> ST s Bool
currentNodeHasType p x =
  currentNode p >>= pure . \case
    Just a -> domNodeType a == x
    Nothing -> False

-- | Determines if the current node has a specified type.
currentNodeHasHTMLType :: Parser s -> BS -> ST s Bool
currentNodeHasHTMLType p = currentNodeHasType p . domMakeTypeHTML

-- | Determines if the current node has a specified type.
currentNodeHasTypeIn :: Parser s -> [DOMType] -> ST s Bool
currentNodeHasTypeIn p x =
  currentNode p >>= pure . \case
    Just a -> domNodeType a `elem` x
    Nothing -> False

-- | Determines if the current node has a specified type.
currentNodeHasHTMLTypeIn :: Parser s -> [BS] -> ST s Bool
currentNodeHasHTMLTypeIn p = currentNodeHasTypeIn p . domTypesHTML

-- | Gets the adjusted current node ID.
adjustedCurrentNodeID :: Parser s -> ST s (Maybe DOMID)
adjustedCurrentNodeID p@Parser {..} = do
  f <- rref parserFragmentMode
  n <- elementStackSize p
  if f && n == 1
     then rref parserContextElement
     else currentNodeID p

-- | Gets the adjusted current node.
adjustedCurrentNode :: Parser s -> ST s (Maybe DOMNode)
adjustedCurrentNode p =
  adjustedCurrentNodeID p >>= maybe (pure Nothing) (getNode p)

-- | Determines if a node is a named MathML element.
isMathMLElementNamed :: DOMNode -> BS -> Bool
isMathMLElementNamed x n = domNodeIsMathML x && domElementName x == n

-- | Determines if a node is a MathML integration point.
isMathMLIntegrationPoint :: DOMNode -> Bool
isMathMLIntegrationPoint x
  | domNodeIsElement x =
      domNodeIsMathML x && Set.member (domElementName x) s
  | otherwise =
      False
  where
    s = Set.fromList [ "mi", "mo", "mn", "ms", "mtext" ]

-- | Determines if a node is a MathML integration point.
isHtmlIntgrationPoint :: DOMNode -> Bool
isHtmlIntgrationPoint x
  | domNodeIsElement x = s || m
  | otherwise = False
  where
    s = domNodeIsSVG x
        && Set.member (domElementName x) s0
    m = domNodeIsMathML x
        && domElementName x == "annotation-xml"
        && case domElementFindAttr x "encoding" of
             Just (DOMAttr n v s) ->
               Set.member (bsLower v) s1
             _otherwise -> False
    s0 = Set.fromList [ "foreignObject", "desc", "title" ]
    s1 = Set.fromList [ "text/html", "application/xhtml+xml" ]

-- | Gets the DOM.
getDOM :: Parser s -> ST s DOM
getDOM Parser {..} = rref parserDOM

-- | Sets the DOM.
setDOM :: Parser s -> DOM -> ST s ()
setDOM Parser {..} = wref parserDOM

-- | Modifies the DOM.
modifyDOM :: Parser s -> (DOM -> DOM) -> ST s ()
modifyDOM p@Parser {..} = uref parserDOM

-- | Sets the insertion mode.
setMode :: Parser s -> ParserMode -> ST s ()
setMode Parser {..} = wref parserInsertionMode

-- | Saves the mode as the original mode.
saveMode :: Parser s -> ST s ()
saveMode Parser {..} = rref parserInsertionMode >>= wref parserOriginalMode

-- | Restore the insertion mode from the saved original mode.
restoreMode :: Parser s -> ST s ()
restoreMode Parser {..} = do
  rref parserOriginalMode >>= wref parserInsertionMode
  wref parserOriginalMode ModeInitial

-- | Sets the current head element.
setHeadID :: Parser s -> Maybe DOMID -> ST s ()
setHeadID Parser {..} = wref parserHeadElement

-- | Gets the current head element.
getHeadID :: Parser s -> ST s (Maybe DOMID)
getHeadID Parser {..} = rref parserHeadElement

-- | Gets the current head element.
getHeadElement :: Parser s -> ST s (Maybe DOMNode)
getHeadElement p = getHeadID p >>= maybe (pure Nothing) (getNode p)

-- | Saves the current node as the head element.
saveHead :: Parser s -> ST s ()
saveHead p = currentNodeID p >>= setHeadID p

-- | Sets the current form element.
setFormID :: Parser s -> Maybe DOMID -> ST s ()
setFormID Parser {..} = wref parserFormElement

-- | Gets the current form element ID.
getFormID :: Parser s -> ST s (Maybe DOMID)
getFormID Parser {..} = rref parserFormElement

-- | Gets the current form element.
getFormElement :: Parser s -> ST s (Maybe DOMNode)
getFormElement p = getFormID p >>= maybe (pure Nothing) (getNode p)

-- | Gets the current form element type.
getFormType :: Parser s -> ST s (Maybe DOMType)
getFormType p@Parser {..} =
  getFormElement p >>= pure . maybe Nothing (Just . domNodeType)

-- | Saves the current node as the form element.
saveForm :: Parser s -> ST s ()
saveForm p = currentNodeID p >>= setFormID p

-- | Determines if the form element reference is defined.
formNotNull :: Parser s -> ST s Bool
formNotNull p = isJust <$> getFormID p

-- | Initializes the self closing flag.
selfClosingInit :: Parser s -> Token -> ST s ()
selfClosingInit p@Parser {..} t =
  wref parserSelfClosingFlag $
    case t of
      TStart {..} -> tStartClosed
      _otherwise -> False

-- | Acknowledges the parser self closing flag.
selfClosingAcknowledge :: Parser s -> ST s ()
selfClosingAcknowledge Parser {..} = wref parserSelfClosingFlag False

-- | Gets the self closing flag.
selfClosingFlag :: Parser s -> ST s Bool
selfClosingFlag Parser {..} = rref parserSelfClosingFlag

-- | Gets the foster parenting flag.
fosterParenting :: Parser s -> ST s Bool
fosterParenting Parser {..} = rref parserFosterParenting

-- | Sets the foster parenting flag.
fosterParentingSet :: Parser s -> ST s ()
fosterParentingSet Parser {..} = wref parserFosterParenting True

-- | Clear the foster parenting flag.
fosterParentingClear :: Parser s -> ST s ()
fosterParentingClear Parser {..} = wref parserFosterParenting False

-- | Sets the frameset flag to not OK.
frameSetNotOK :: Parser s -> ST s ()
frameSetNotOK Parser {..} = wref parserFrameSetOK False

-- | Gets the iframe srcdoc flag.
iframeSrcDoc :: Parser s -> ST s Bool
iframeSrcDoc Parser {..} = rref parserIFrameSrcDoc

-- | Sets the done flag.
parserSetDone :: Parser s -> ST s ()
parserSetDone Parser {..} = wref parserDone True

-- | Gets the active format list.
activeFormatList :: Parser s -> ST s [ParserFormatItem]
activeFormatList Parser {..} = rref parserActiveFormatList

-- | Gets the names of the active format elements.
activeFormatNames :: Parser s -> ST s [BS]
activeFormatNames p = do
  d <- getDOM p
  map (f d) <$> activeFormatList p
  where f d ParserFormatMarker = "marker"
        f d (ParserFormatElement i t) =
          domElementName $ fromJust $ domGetNode d i

-- | Adds a marker to the list of active format elements.
activeFormatAddMarker :: Parser s -> ST s ()
activeFormatAddMarker Parser {..} =
  uref parserActiveFormatList (ParserFormatMarker:)

-- | Limits matching active format elements up to the first
-- marker to two so that a third can be added (Noah's Ark clause).
activeFormatLimit :: (DOMID -> Bool) -> [ParserFormatItem] -> [ParserFormatItem]
activeFormatLimit match xs =
  f 0 xs
  where
    f :: Int -> [ParserFormatItem] -> [ParserFormatItem]
    f n [] = []
    f n (e@(ParserFormatMarker):es) = e : es
    f n (e@(ParserFormatElement domId _):es)
      | match domId = if n < 2 then e : f (n + 1) es else f n es
      | otherwise = e : f n es

-- | Adds an element to the list of active format elements.
activeFormatAddElement :: Parser s -> Token -> DOMID -> ST s ()
activeFormatAddElement p@Parser {..} t x = do
  d <- getDOM p
  a <- activeFormatList p
  wref parserActiveFormatList $
    ParserFormatElement x t : activeFormatLimit (domMatch d x) a

-- | Adds the current node to the list of active format elements.
activeFormatAddCurrentNode :: Parser s -> Token -> ST s ()
activeFormatAddCurrentNode p@Parser {..} t =
  whenJustM (currentNodeID p) $ activeFormatAddElement p t

-- | Determines if any format elements up to a marker satisfy a predicate.
activeFormatAny :: Parser s -> (DOMNode -> Bool) -> ST s Bool
activeFormatAny p@Parser {..} f = do
  d <- getDOM p
  a <- activeFormatList p
  pure $
    ( any f
    . domMapID d
    . mapMaybe g
    . takeWhile (not . formatItemIsMarker)
    ) a
  where
    g :: ParserFormatItem -> Maybe DOMID
    g = \case
      ParserFormatElement x _ ->
        Just x
      ParserFormatMarker ->
        Nothing

-- | Determines if the active format list contains an element.
activeFormatContains :: Parser s -> DOMID -> ST s Bool
activeFormatContains p x = any (formatItemHasID x) <$> activeFormatList p

-- | Finds a format item with a specified tag name.
activeFormatFindTag :: Parser s -> BS -> ST s (Maybe ParserFormatItem)
activeFormatFindTag p@Parser {..} x = do
  d <- getDOM p
  a <- activeFormatList p
  pure $
    ( find (formatItemHasTag d x)
    . takeWhile (not . formatItemIsMarker)
    ) a

-- | Finds the token for a node ID.
activeFormatFindToken :: Parser s -> DOMID -> ST s (Maybe Token)
activeFormatFindToken p@Parser {..} x =
  activeFormatList p >>= f
  where
    f [] = pure Nothing
    f ((ParserFormatMarker):xs) = f xs
    f ((ParserFormatElement i t):xs)
      | x == i = pure $ Just t
      | otherwise = f xs

-- | Reconstructs the list of active format elements.
activeFormatReconstruct :: Parser s -> ST s ()
activeFormatReconstruct p = do
  e <- elementStack p
  a <- activeFormatList p
  case a of
    [] -> pure ()
    (x:xs)
      | openOrMarker e x -> pure ()
      | otherwise -> do
          let b = reverse . takeWhile (not . openOrMarker e) $ a
              a' = drop (length b) a
          reopen p b a'

-- | Determines is a format item is open.
openOrMarker :: [DOMID] -> ParserFormatItem -> Bool
openOrMarker x = \case
   ParserFormatMarker -> True
   ParserFormatElement i _ -> i `elem` x

-- | Reopens a format item.
reopen :: Parser s -> [ParserFormatItem] -> [ParserFormatItem] -> ST s ()
reopen p@Parser {..} b a =
  case b of
    [] ->
      wref parserActiveFormatList a
    ((ParserFormatMarker):xs) ->
      reopen p xs a
    ((ParserFormatElement _ t):xs) -> do
      insertHtmlElement p t
      i <- fromJust <$> currentNodeID p
      reopen p xs $ ParserFormatElement i t : a

-- | Clears the list of active format elements up to last marker.
activeFormatClear :: Parser s -> ST s ()
activeFormatClear p =
  activeFormatModify p $ drop 1 . dropWhile (not . formatItemIsMarker)

-- | Removes a node from the active format element list.
activeFormatRemove :: Parser s -> DOMID -> ST s ()
activeFormatRemove p x =
  activeFormatModify p $ filter $ not . formatItemHasID x

-- | Replaces an ID in the active format list.
activeFormatReplace :: Parser s -> DOMID -> DOMID -> ST s ()
activeFormatReplace p x y =
  activeFormatModify p $ map f
  where
    f z@(ParserFormatMarker) = z
    f z@(ParserFormatElement i t)
      | i == x = ParserFormatElement y t
      | otherwise = z

-- | Modifies the active format list.
activeFormatModify :: Parser s -> ([ParserFormatItem] -> [ParserFormatItem]) -> ST s ()
activeFormatModify Parser {..} = uref parserActiveFormatList

-- | Gets the active format successor for an ID.
activeFormatSucc :: Parser s -> DOMID -> ST s (Maybe DOMID)
activeFormatSucc p x =
  f <$> activeFormatList p
  where
    f a = case findSucc (formatItemHasID x) a of
      Just (ParserFormatElement i _) -> Just i
      _otherwise -> Nothing

-- | Inserts an element in the active format list.
activeFormatInsertElement :: Parser s -> DOMID -> Token -> Maybe DOMID -> ST s ()
activeFormatInsertElement p x t y =
  case y of
    Just a -> activeFormatModify p $ insertBefore (formatItemHasID a) e
    Nothing -> activeFormatModify p (<>[e])
  where
    e = ParserFormatElement x t

-- | Determines if a format item is a marker.
formatItemIsMarker :: ParserFormatItem -> Bool
formatItemIsMarker ParserFormatMarker = True
formatItemIsMarker (ParserFormatElement _ _) = False

-- | Determines if a format item has the specified ID.
formatItemHasID :: DOMID -> ParserFormatItem -> Bool
formatItemHasID x ParserFormatMarker = False
formatItemHasID x (ParserFormatElement i _) = i == x

-- | Determines if a format item has a certain tag name.
formatItemHasTag :: DOM -> BS -> ParserFormatItem -> Bool
formatItemHasTag d n ParserFormatMarker = False
formatItemHasTag d n (ParserFormatElement i _) =
  case domGetNode d i of
    Just x -> domNodeElementName x == n
    Nothing -> False

-- | Gets the current template insertion mode.
templateModeCurrent :: Parser s -> ST s (Maybe ParserMode)
templateModeCurrent p@Parser {..} = listToMaybe <$> rref parserTemplateMode

-- | Pushes an insertion mode onto the stack of template insertion modes.
templateModePush :: Parser s -> ParserMode -> ST s ()
templateModePush p@Parser {..} x = uref parserTemplateMode (x:)

-- | Pops an insertion mode off of the stack of template insertion modes.
templateModePop :: Parser s -> ST s ()
templateModePop p@Parser {..} =
  rref parserTemplateMode >>= \case
    (x:xs) -> wref parserTemplateMode xs
    []     -> parseError p Nothing "attempt to pop empty template mode stack"

-- | Gets the current number of template modes.
templateModeCount :: Parser s -> ST s Int
templateModeCount p@Parser {..} = length <$> rref parserTemplateMode

-- | Gets the appropriate insertion location.
appropriateInsertionLocation :: Parser s -> Maybe DOMID -> ST s DOMPos
appropriateInsertionLocation p@Parser {..} override = do
  -- (1) Check for override target.
  target <- case override of
    Just a -> pure a
    Nothing -> maybe domRoot id <$> currentNodeID p
  getNode p target >>= \case
    Nothing ->
      pure $ DOMPos domRoot Nothing
    Just n -> do
      f <- fosterParenting p
      -- (2) Determine the adjusted insertion location.
      adjusted <-
        if f && domNodeElementName n `elem`
          [ "table", "tbody", "tfoot", "thead", "tr" ]
        then do
          -- (2.1) Get last template in element stack.
          lastTemplate <- elementStackFind p $ \x ->
            elementDetailsType x == domMakeTypeHTML "template"
          -- (2.2) Get last table in element stack.
          lastTable <- elementStackFind p $ \x ->
            elementDetailsType x == domMakeTypeHTML "table"

          -- (2.3) Check for template and no table.
          if | Just (ElementDetails i1 _ n1 _) <- lastTemplate
             , Just (ElementDetails i2 _ _ _) <- lastTable
             , i1 < i2 -> do
                 pure $ DOMPos (domTemplateContents n1) Nothing

             | Just (ElementDetails _ _ n1 _) <- lastTemplate
             , Nothing <- lastTable -> do
                 pure $ DOMPos (domTemplateContents n1) Nothing

             | otherwise -> case lastTable of
                 Nothing -> do
                   -- (2.4) If no last table then use first element.
                   j <- fromJust <$> lastNodeID p
                   pure $ DOMPos j Nothing
                 Just (ElementDetails _ x2 n2 _) -> do
                   -- (2.5) Check last table parent node.
                   if | domNodeParent n2 /= domNull ->
                          pure $ DOMPos (domNodeParent n2) $ Just x2
                      | otherwise -> do
                          -- (2.6) Previous element is above last table.
                          prev <- fromJust <$> elementStackSucc p x2
                          -- (2.7) Location is after previous element last child.
                          pure $ DOMPos prev Nothing
        else
          pure $ DOMPos target Nothing
      getNode p (domPosParent adjusted) >>= \case
        Just DOMTemplate{..} ->
          -- (3) Use template contents instead.
          pure $ DOMPos domTemplateContents Nothing
        _otherwise ->
          -- (4) Return adjusted insertion location.
          pure adjusted

-- | Gets the appropriate insertion location.
insertionLocation :: Parser s -> ST s DOMPos
insertionLocation p = appropriateInsertionLocation p Nothing

-- | Creates an element for a token.
-- The standard describes a much more involved process than
-- what is used here (refer to 12.2.5.1).
createElementForToken :: Parser s -> Token -> HTMLNamespace -> ST s DOMID
createElementForToken p t s
  | tStartName t == "template" = do
      i <- newID p $ domDefaultFragment
      j <- newID p $ DOMTemplate
        { domTemplateID         = domNull
        , domTemplateNamespace  = s
        , domTemplateAttributes = Seq.empty
        , domTemplateContents   = i
        , domTemplateParent     = domNull
        }
      modifyDOM p $ domSetParent i j
      pure j
  | otherwise = do
      i <- newID p $ DOMElement
        { domElementID         = domNull
        , domElementName       = tStartName t
        , domElementNamespace  = s
        , domElementAttributes = Seq.fromList $ map f (tStartAttr t)
        , domElementChildren   = Seq.empty
        , domElementParent     = domNull
        }
      pure i
  where
    f (TAttr n v s) = DOMAttr n v s

-- | Inserts a foreign element into the document.
insertForeignElement :: Parser s -> HTMLNamespace -> Token -> ST s ()
insertForeignElement p n =
  withStartToken $ \t -> do
    i <- createElementForToken p t n
    x <- insertionLocation p
    modifyDOM p $ domInsert x i
    elementStackPush p i

-- | Inserts an HTML element into the document.
insertHtmlElement :: Parser s -> Token -> ST s ()
insertHtmlElement p = insertForeignElement p HTMLNamespaceHTML

-- | Inserts a MathML element into the document.
insertMathMLElement :: Parser s -> Token -> ST s ()
insertMathMLElement p = insertForeignElement p HTMLNamespaceMathML

-- | Inserts an SVG element into the document.
insertSvgElement :: Parser s -> Token -> ST s ()
insertSvgElement p = insertForeignElement p HTMLNamespaceSVG

-- | Inserts an HTML element into the document.
insertHtmlElementNamed :: Parser s -> BS -> ST s ()
insertHtmlElementNamed p x = insertHtmlElement p $ TStart x False []

-- | Adjusts the MathML attributes for a token.
adjustAttrMathML :: Token -> Token
adjustAttrMathML t =
  case t of
    TStart {} -> t { tStartAttr = map f $ tStartAttr t }
    _otherwise -> t
  where
    f (TAttr n v s) = TAttr (g n) v s
    g x = if x == "definitionurl" then "definitionUrl" else x

-- | Adjusts the SVG attributes for a token.
adjustAttrSVG :: Token -> Token
adjustAttrSVG token =
  case token of
    TStart {..} ->
      token { tStartAttr = map f tStartAttr }
    _otherwise ->
      token
  where
    f a@(TAttr n v s) =
      case Map.lookup n svgAttributeMap of
        Just n' -> TAttr n' v s
        Nothing -> a

-- | Adjusts the foreign attributes for a token.
adjustAttrForeign :: Token -> Token
adjustAttrForeign token =
  case token of
    TStart {..} ->
      token { tStartAttr = map f tStartAttr }
    _otherwise ->
      token
  where
    f a@(TAttr n v s) =
      case Map.lookup n foreignAttributeMap of
        Just (n', s') -> TAttr n' v s'
        Nothing -> a

-- | Adjusts the element name for an SVG element.
adjustElemSVG :: Token -> Token
adjustElemSVG token =
  case token of
    TStart {..} ->
      case Map.lookup tStartName svgElementMap of
        Just x -> token { tStartName = x }
        Nothing -> token
    _otherwise ->
      token

-- | Adjustable SVG attribute map.
svgAttributeMap :: Map BS BS
svgAttributeMap = Map.fromList
  [ ("attributename", "attributeName")
  , ("attributetype", "attributeType")
  , ("basefrequency", "baseFrequency")
  , ("baseprofile", "baseProfile")
  , ("calcmode", "calcMode")
  , ("clippathunits", "clipPathUnits")
  , ("diffuseconstant", "diffuseConstant")
  , ("edgemode", "edgeMode")
  , ("filterunits", "filterUnits")
  , ("glyphref", "glyphRef")
  , ("gradienttransform", "gradientTransform")
  , ("gradientunits", "gradientUnits")
  , ("kernelmatrix", "kernelMatrix")
  , ("kernelunitlength", "kernelUnitLength")
  , ("keypoints", "keyPoints")
  , ("keysplines", "keySplines")
  , ("keytimes", "keyTimes")
  , ("lengthadjust", "lengthAdjust")
  , ("limitingconeangle", "limitingConeAngle")
  , ("markerheight", "markerHeight")
  , ("markerunits", "markerUnits")
  , ("markerwidth", "markerWidth")
  , ("maskcontentunits", "maskContentUnits")
  , ("maskunits", "maskUnits")
  , ("numoctaves", "numOctaves")
  , ("pathlength", "pathLength")
  , ("patterncontentunits", "patternContentUnits")
  , ("patterntransform", "patternTransform")
  , ("patternunits", "patternUnits")
  , ("pointsatx", "pointsAtX")
  , ("pointsaty", "pointsAtY")
  , ("pointsatz", "pointsAtZ")
  , ("preservealpha", "preserveAlpha")
  , ("preserveaspectratio", "preserveAspectRatio")
  , ("primitiveunits", "primitiveUnits")
  , ("refx", "refX")
  , ("refy", "refY")
  , ("repeatcount", "repeatCount")
  , ("repeatdur", "repeatDur")
  , ("requiredextensions", "requiredExtensions")
  , ("requiredfeatures", "requiredFeatures")
  , ("specularconstant", "specularConstant")
  , ("specularexponent", "specularExponent")
  , ("spreadmethod", "spreadMethod")
  , ("startoffset", "startOffset")
  , ("stddeviation", "stdDeviation")
  , ("stitchtiles", "stitchTiles")
  , ("surfacescale", "surfaceScale")
  , ("systemlanguage", "systemLanguage")
  , ("tablevalues", "tableValues")
  , ("targetx", "targetX")
  , ("targety", "targetY")
  , ("textlength", "textLength")
  , ("viewbox", "viewBox")
  , ("viewtarget", "viewTarget")
  , ("xchannelselector", "xChannelSelector")
  , ("ychannelselector", "yChannelSelector")
  , ("zoomandpan", "zoomAndPan")
  ]

-- | Adjustable SVG element map.
svgElementMap :: Map BS BS
svgElementMap = Map.fromList
  [ ("altglyph", "altGlyph")
  , ("altglyphdef", "altGlyphDef")
  , ("altglyphitem", "altGlyphItem")
  , ("animatecolor", "animateColor")
  , ("animatemotion", "animateMotion")
  , ("animatetransform", "animateTransform")
  , ("clippath", "clipPath")
  , ("feblend", "feBlend")
  , ("fecolormatrix", "feColorMatrix")
  , ("fecomponenttransfer", "feComponentTransfer")
  , ("fecomposite", "feComposite")
  , ("feconvolvematrix", "feConvolveMatrix")
  , ("fediffuselighting", "feDiffuseLighting")
  , ("fedisplacementmap", "feDisplacementMap")
  , ("fedistantlight", "feDistantLight")
  , ("fedropshadow", "feDropShadow")
  , ("feflood", "feFlood")
  , ("fefunca", "feFuncA")
  , ("fefuncb", "feFuncB")
  , ("fefuncg", "feFuncG")
  , ("fefuncr", "feFuncR")
  , ("fegaussianblur", "feGaussianBlur")
  , ("feimage", "feImage")
  , ("femerge", "feMerge")
  , ("femergenode", "feMergeNode")
  , ("femorphology", "feMorphology")
  , ("feoffset", "feOffset")
  , ("fepointlight", "fePointLight")
  , ("fespecularlighting", "feSpecularLighting")
  , ("fespotlight", "feSpotLight")
  , ("fetile", "feTile")
  , ("feturbulence", "feTurbulence")
  , ("foreignobject", "foreignObject")
  , ("glyphref", "glyphRef")
  , ("lineargradient", "linearGradient")
  , ("radialgradient", "radialGradient")
  , ("textpath", "textPath")
  ]

-- | Map of foreign attribute adjustments.
foreignAttributeMap :: Map BS (BS, HTMLAttrNamespace)
foreignAttributeMap = Map.fromList
  [ ("xlink:actuate", ("actuate", HTMLAttrNamespaceXLink))
  , ("xlink:arcrole", ("arcrole", HTMLAttrNamespaceXLink))
  , ("xlink:href", ("href", HTMLAttrNamespaceXLink))
  , ("xlink:role", ("role", HTMLAttrNamespaceXLink))
  , ("xlink:show", ("show", HTMLAttrNamespaceXLink))
  , ("xlink:title", ("title", HTMLAttrNamespaceXLink))
  , ("xlink:type", ("type", HTMLAttrNamespaceXLink))
  , ("xml:lang", ("lang", HTMLAttrNamespaceXML))
  , ("xml:space", ("space", HTMLAttrNamespaceXML))
  , ("xmlns", ("xmlns", HTMLAttrNamespaceXMLNS))
  , ("xmlns:xlink", ("xlink", HTMLAttrNamespaceXMLNS))
  ]

-- | Inserts a node as a child of another node.
insertNode :: Parser s -> DOMPos -> DOMID -> ST s ()
insertNode p@Parser {..} i x = modifyDOM p $ domInsert i x

-- | Inserts a new node as a child of another node.
insertNewNode :: Parser s -> DOMPos -> DOMNode -> ST s DOMID
insertNewNode p@Parser {..} i x = do
  d <- getDOM p
  let (d', j) = domInsertNew i x d
  setDOM p d'
  pure j

-- | Inserts a node as a child of the document.
insertDocumentNode :: Parser s -> DOMID -> ST s ()
insertDocumentNode p@Parser {..} = insertNode p domRootPos

-- | Inserts a new node as a child of the document.
insertNewDocumentNode :: Parser s -> DOMNode -> ST s ()
insertNewDocumentNode p@Parser {..} = void . insertNewNode p domRootPos

-- | Makes a comment node.
commentMake :: Parser s -> Token -> ST s DOMNode
commentMake p@Parser {..} t =
  pure DOMComment
    { domCommentID     = domNull
    , domCommentData   = tCommentData t
    , domCommentParent = domNull
    }

-- | Makes a document type node.
doctypeMake :: Parser s -> Token -> ST s DOMNode
doctypeMake p@Parser {..} = pure . \case
  TDoctype {..} ->
    DOMDoctype
      { domDoctypeID       = domNull
      , domDoctypeName     = tDoctypeName
      , domDoctypePublicID = tDoctypePublic
      , domDoctypeSystemID = tDoctypeSystem
      , domDoctypeParent   = domNull
      }
  _otherwise ->
    domDefaultDoctype

-- | Inserts a new comment in the document.
insertComment :: Parser s -> Token -> ST s ()
insertComment p@Parser {..} t =
  insertionLocation p >>= \x ->
    commentMake p t >>= void . insertNewNode p x

-- | Inserts a new comment as child of the document node.
insertDocComment :: Parser s -> Token -> ST s ()
insertDocComment p@Parser {..} t =
  commentMake p t >>= void . insertNewNode p domRootPos

-- | Inserts a new character in the document.
insertChar :: Parser s -> Token -> ST s ()
insertChar p@Parser {..} =
  withCharToken $ \w -> do
    pos <- insertionLocation p
    let i = domPosParent pos
    when (i /= domRoot) $ do
      d <- getDOM p
      case domLastChild d i of
        Nothing -> do
          j <- insertNewNode p pos domDefaultText
          textMapAppend p j w
        Just x ->
          case domGetNode d x of
            Just n@DOMText{..} ->
              textMapAppend p domTextID w
            Just n -> do
              j <- insertNewNode p pos domDefaultText
              textMapAppend p j w
            Nothing ->
              parseError p Nothing $ "insert char bad id: " <> bcPack (show x)

-- | Appends a word to a text node buffer.
textMapAppend :: Parser s -> DOMID -> Word8 -> ST s ()
textMapAppend Parser {..} i w = do
  m <- rref parserTextMap
  case IntMap.lookup i m of
    Just b ->
      bufferAppend w b
    Nothing -> do
      b <- bufferNew
      bufferAppend w b
      wref parserTextMap $ IntMap.insert i b m

-- | Finds a buffered string in the text map.
textMapLookup :: Parser s -> DOMID -> ST s BS
textMapLookup Parser {..} i = do
  m <- rref parserTextMap
  case IntMap.lookup i m of
    Just b  -> bufferPack b
    Nothing -> pure bsEmpty

-- | Returns a dom with the text nodes populated with text values.
textMapDOM :: Parser s -> ST s DOM
textMapDOM p@Parser {..} = do
  DOM{..} <- getDOM p
  m <- rref parserTextMap >>= mapM bufferPack
  let f x = IntMap.findWithDefault bsEmpty x m
      a = flip IntMap.mapWithKey domNodes $ \i n ->
             case n of
               DOMText{} -> n { domTextData = f i }
               _otherwise -> n
  pure $ DOM a domNextID

-- | Invokes processing for a start tag token token.
withStartToken :: (Token -> ST s ()) -> Token -> ST s ()
withStartToken f = \case
  t@TStart {} -> f t
  _otherwise -> pure ()

-- -- | Invokes processing for a character token.
withCharToken :: (Word8 -> ST s ()) -> Token -> ST s ()
withCharToken f = \case
  TChar w    -> f w
  _otherwise -> pure ()

-- | Updates the parser lexer using a combinator.
parserLexerUpdate :: Parser s -> (Lexer s -> ST s ()) -> ST s ()
parserLexerUpdate Parser {..} f = rref parserLexer >>= f

-- | Sets the lexer to skip next linefeed.
parserSkipNextLF :: Parser s -> ST s ()
parserSkipNextLF p = parserLexerUpdate p lexerSkipNextLF

-- | Sets the lexer to RCDATA mode.
parserSetRCDATA :: Parser s -> ST s ()
parserSetRCDATA p = parserLexerUpdate p lexerSetRCDATA

-- | Sets the lexer to raw text mode.
parserSetRAWTEXT :: Parser s -> ST s ()
parserSetRAWTEXT p = parserLexerUpdate p lexerSetRAWTEXT

-- | Sets the lexer to plaintext mode.
parserSetPLAINTEXT :: Parser s -> ST s ()
parserSetPLAINTEXT p = parserLexerUpdate p lexerSetPLAINTEXT

-- | Sets the lexer to script data mode.
parserSetScriptData :: Parser s -> ST s ()
parserSetScriptData p = parserLexerUpdate p lexerSetScriptData

-- | Inserts an RCDATA text element.
insertElementRCDATA :: Parser s -> Token -> ST s ()
insertElementRCDATA p t = do
  insertHtmlElement p t
  parserSetRCDATA p
  saveMode p
  setMode p ModeText

-- | Inserts a raw text element.
insertElementRAWTEXT :: Parser s -> Token -> ST s ()
insertElementRAWTEXT p t = do
  insertHtmlElement p t
  parserSetRAWTEXT p
  saveMode p
  setMode p ModeText

-- | Generates the implied end tags.
generateImpliedEndTags :: Parser s -> ST s ()
generateImpliedEndTags p = generateImpliedEndTagsExcept p bsEmpty

-- | Generates the implied end tags with an exception.
generateImpliedEndTagsExcept :: Parser s -> BS -> ST s ()
generateImpliedEndTagsExcept p x =
  elementStackPopWhile p $ elementNameIn $
    filter (/=x) [ "dd", "dt", "li", "menuitem", "optgroup",
                   "option", "p", "rb", "rp", "rt", "rtc" ]

-- | Generates the implied end tags.
generateImpliedEndTagsThoroughly :: Parser s -> ST s ()
generateImpliedEndTagsThoroughly p =
  elementStackPopWhile p $ elementNameIn
    [ "caption", "colgroup", "dd", "dt", "li", "optgroup",
      "option", "p", "rb", "rp", "rt", "rtc",
      "tbody", "td", "tfoot", "th", "thead", "tr" ]

-- | Resets the insertion mode appropriately.
resetInsertionMode :: Parser s -> ST s ()
resetInsertionMode p@Parser {..} =
  elementStackNodes p >>= f
  where
    f [] = pure ()
    f (x:xs) = do
      x' <- node
      case (domNodeElementName x', lastNode) of
        ("select", _)   -> g (x':xs)
        ("td", False)   -> setMode p ModeInCell
        ("th", False)   -> setMode p ModeInCell
        ("tr", _)       -> setMode p ModeInRow
        ("tbody", _)    -> setMode p ModeInTableBody
        ("thead", _)    -> setMode p ModeInTableBody
        ("tfoot", _)    -> setMode p ModeInTableBody
        ("caption", _)  -> setMode p ModeInCaption
        ("colgroup", _) -> setMode p ModeInColumnGroup
        ("table", _)    -> setMode p ModeInTable
        ("head", False) -> setMode p ModeInHead
        ("body", _)     -> setMode p ModeInBody
        ("frameset", _) -> setMode p ModeInFrameset
        ("template", _) -> templateModeCurrent p >>= \case
                             Just m  -> setMode p m
                             Nothing -> pure ()
        ("html", _)     -> getHeadID p >>= \case
                             Nothing -> setMode p ModeBeforeHead
                             Just _  -> setMode p ModeAfterHead
        (_, True)       -> setMode p ModeInBody
        (_, False)      -> f xs
      where
        lastNode = length xs == 0
        node = do
          a <- rref parserFragmentMode
          c <- rref parserContextElement
          n <- getNode p $ fromJust c
          pure $
            if lastNode && a && isJust c
               then fromJust $ n
               else x

    g (x:[]) =
      setMode p ModeInSelect
    g (x:y:ys) =
      case domNodeElementName y of
        "template" -> setMode p ModeInSelect
        "table"    -> setMode p ModeInSelectInTable
        _otherwise -> g (y:ys)

-- | Closes a P element.
closeElementP :: Parser s -> ST s ()
closeElementP p = do
  let t = domMakeTypeHTML "p"
  generateImpliedEndTagsExcept p "p"
  unlessM (currentNodeHasType p t) $
    parseError p Nothing "current node not p when closing p element"
  elementStackPopUntilType p t

-- | Defines the adoption agency state.
data ParserAdoptionAgency s = ParserAdoptionAgency
  { aaSubject            :: BS
  , aaOuterLoopCount     :: Int
  , aaInnerLoopCount     :: Int
  , aaNode               :: DOMID
  , aaLastNode           :: DOMID
  , aaNextNode           :: DOMID
  , aaFormattingElement  :: DOMID
  , aaCommonAncestor     :: DOMID
  , aaFurthestBlock      :: DOMID
  , aaBookmark           :: (Maybe DOMID)
  , aaAnyOtherEndTag     :: ST s ()
  }

-- | Defines the default adoption agency state.
defaultAA :: ST s (ParserAdoptionAgency s)
defaultAA =
  pure $ ParserAdoptionAgency
    { aaSubject            = bsEmpty
    , aaOuterLoopCount     = 0
    , aaInnerLoopCount     = 0
    , aaNode               = domNull
    , aaLastNode           = domNull
    , aaNextNode           = domNull
    , aaFormattingElement  = domNull
    , aaCommonAncestor     = domNull
    , aaFurthestBlock      = domNull
    , aaBookmark           = Nothing
    , aaAnyOtherEndTag     = pure ()
    }

-- | Modifies the adoption agency state.
modifyAA :: Parser s -> (ParserAdoptionAgency s -> ParserAdoptionAgency s) -> ST s ()
modifyAA Parser {..} = uref parserAdoptionAgency

-- | Gets the adoption agency state.
getAA :: Parser s -> ST s (ParserAdoptionAgency s)
getAA Parser {..} = rref parserAdoptionAgency

-- | Gets a field from the adoption agency state.
getsAA :: Parser s -> (ParserAdoptionAgency s -> a) -> ST s a
getsAA p f = f <$> getAA p

-- | Runs the adoption agency algorithm.
adoptionAgencyRun :: Parser s -> BS -> ST s () -> ST s ()
adoptionAgencyRun p@Parser {..} subject anyOther = do
  a <- currentNodeHasType p $ domMakeTypeHTML subject
  b <- currentNodeID p >>= \case
    Just i -> notM $ activeFormatContains p i
    Nothing -> pure True
  unless (a && b) $ do
    aa <- defaultAA
    modifyAA p $ const aa
      { aaSubject        = subject
      , aaAnyOtherEndTag = anyOther
      }
    adoptionAgencyOuterLoop p

-- | Runs the outer loop portion of the adoption agency algorithm.
adoptionAgencyOuterLoop :: Parser s -> ST s ()
adoptionAgencyOuterLoop p = do
  i <- getsAA p aaOuterLoopCount
  -- (3) Check outer loop counter.
  when (i < 8) $ do
    -- (4) Increment outer loop counter.
    modifyAA p $ \a -> a { aaOuterLoopCount = aaOuterLoopCount a + 1 }
    -- (5) Find the formatting element.
    liftA aaSubject (getAA p) >>= activeFormatFindTag p >>= \case
      Nothing -> do
        doAnyOtherEndTag <- getsAA p aaAnyOtherEndTag
        doAnyOtherEndTag
      Just (ParserFormatElement fe t) -> do
        modifyAA p $ \a -> a { aaFormattingElement = fe }
        -- Geting formatting node
        x <- fromJust <$> getNode p fe
        let name = domElementName x
        -- (6) Check if formatting element is not in element stack.
        (elementStackAny p ((==) fe . domNodeID)) >>= \case
          False -> do
            parseError p Nothing $
              "element stack missing " <> name
              <> "(ID:" <> bcPack (show fe) <> ") during adoption"
            activeFormatRemove p fe
          True ->
            -- (7) Check if formatting element is not in scope.
            (elementInScope p $ domNodeType x) >>= \case
              False ->
                parseError p Nothing $
                  "element " <> name <> " not in scope during adoption"
              True -> do
                -- (8) Check if formatting element is not current node.
                unlessM (maybe False (==fe) <$> currentNodeID p) $
                  parseError p Nothing $ "element " <> name
                    <> " is not the current ID during adoption"
                -- (9) Find the furthest block.
                d <- getDOM p
                f <- pure $ find $ elementIsSpecial
                  . domNodeType . fromJust . domGetNode d
                liftA (f . reverse . takeWhile (/=fe)) (elementStack p) >>= \case
                  Nothing -> do
                    -- (10) There was no furthest block.
                    elementStackPopUntilID p fe
                    activeFormatRemove p fe
                  Just fb -> do
                    -- (11) Find the common ancestor.
                    ca <- fromJust <$> elementStackSucc p fe
                    -- (12) Bookmark notes position of formatting element.
                    bm <- activeFormatSucc p fe
                    modifyAA p $ \a -> a
                      { aaNode = fb
                      , aaLastNode = fb
                      , aaCommonAncestor = ca
                      , aaFurthestBlock = fb
                      , aaBookmark = bm
                      }
                    adoptionAgencyInnerLoop p

-- | Runs the inner loop portion of the adoption agency algorithm.
adoptionAgencyInnerLoop :: Parser s -> ST s ()
adoptionAgencyInnerLoop p = do
  -- (13.2) Increment inner loop counter.
  modifyAA p $ \a -> a { aaInnerLoopCount = aaInnerLoopCount a + 1 }
  -- (13.3) Move to the next node.
  n <- getsAA p aaNode
  m <- getsAA p aaNextNode
  node <- maybe m id <$> elementStackSucc p n
  -- (13.4) Check if node is the formatting element.
  f <- getsAA p aaFormattingElement
  if node == f
     then adoptionAgencyPostLoop p
     else do
       -- (13.5) Check the inner loop counter.
       ic <- getsAA p aaInnerLoopCount
       ac <- activeFormatContains p node
       when (ic > 3 && ac) $ activeFormatRemove p node
       -- (13.6) Check if node is in the active format list.
       unlessM (activeFormatContains p node) $ do
         m <- fromJust <$> elementStackSucc p node
         modifyAA p $ \a -> a { aaNextNode = m }
         elementStackRemove p node
         adoptionAgencyInnerLoop p
       -- (13.7) Create an element for the token for node.
       t <- fromJust <$> activeFormatFindToken p node
       e <- createElementForToken p t HTMLNamespaceHTML
       c <- getsAA p aaCommonAncestor
       modifyDOM p $ domAppend c e
       activeFormatReplace p node e
       elementStackReplace p node e
       modifyAA p $ \a -> a { aaNode = e }
       -- (13.8) Check if node is the furthest block.
       x <- getsAA p aaLastNode
       b <- getsAA p aaFurthestBlock
       when (x == b) $ do
         bm <- activeFormatSucc p e
         modifyAA p $ \a -> a { aaBookmark = bm }
       -- (13.9) Insert last node into node.
       modifyDOM p $ domMove x e
       -- (13.10) Set last node to be node.
       modifyAA p $ \a -> a { aaLastNode = e }
       -- (13.11) Repeat inner loop.
       adoptionAgencyInnerLoop p

-- | Runs the post-loop portion of the adoption agency algorithm.
adoptionAgencyPostLoop :: Parser s -> ST s ()
adoptionAgencyPostLoop p = do
  -- (14) Insert last node with common ancestor as target.
  c <- getsAA p aaCommonAncestor
  n <- getsAA p aaLastNode
  i <- appropriateInsertionLocation p $ Just c
  modifyDOM p $ domMove n $ domPosParent i
  -- (15) Create element for token.
  f <- getsAA p aaFormattingElement
  t <- fromJust <$> activeFormatFindToken p f
  e <- createElementForToken p t HTMLNamespaceHTML
  -- (16) Move furthest block children to new element.
  b <- getsAA p aaFurthestBlock
  modifyDOM p $ domMoveChildren b e
  -- (17) Append new element to furthest block.
  modifyDOM p $ domAppend b e
  -- (18) Remove formatting element and insert new element.
  activeFormatRemove p f
  getsAA p aaBookmark >>= activeFormatInsertElement p e t
  -- (19) Remove formatting element and insert new element.
  elementStackRemove p f
  elementStackInsertBefore p b e
  -- (20) Jump back to outer loop.
  adoptionAgencyOuterLoop p

-- | Initializes the pending table characters.
pendingTableCharInit :: Parser s -> ST s ()
pendingTableCharInit Parser {..} = wref parserTableChars []

-- | Appends a character to the pending table characters.
pendingTableCharAppend :: Parser s -> Token -> ST s ()
pendingTableCharAppend Parser {..} t = uref parserTableChars (<>[t])

-- | Gets the pending table characters.
pendingTableChars :: Parser s -> ST s [Token]
pendingTableChars Parser {..} = rref parserTableChars

-- | Checks the DOCTYPE token for validity.
doctypeTokenCheck :: Parser s -> Token -> ST s ()
doctypeTokenCheck parser@Parser {..} t@(TDoctype n q p s) =
  when (n /= "html"
    || p /= Nothing
    || s /= Nothing && s /= Just "about:legacy-compat") $
    parseError parser (Just t) "doctype error"

-- | Determines if the doctype token represents quirks mode.
tokenQuirks :: Token -> Bool
tokenQuirks (TDoctype n True p s) = True
tokenQuirks (TDoctype n False p s) =
  or
  [ n /= "html"
  , idMatch p "-//W3O//DTD W3 HTML Strict 3.0//EN//"
  , idMatch p "-/W3C/DTD HTML 4.0 Transitional/EN"
  , idMatch p "HTML"
  , idMatch s "http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd"
  , anyPrefix p publicIdPrefix
  , and
    [ s == Nothing
    , anyPrefix p
      [ "-//W3C//DTD HTML 4.01 Frameset//"
      , "-//W3C//DTD HTML 4.01 Transitional//"
      ]
    ]
  ]

-- | Public ID prefixes.
publicIdPrefix :: [BS]
publicIdPrefix =
  [ "+//Silmaril//dtd html Pro v0r11 19970101//"
  , "-//AS//DTD HTML 3.0 asWedit + extensions//"
  , "-//AdvaSoft Ltd//DTD HTML 3.0 asWedit + extensions//"
  , "-//IETF//DTD HTML 2.0 Level 1//"
  , "-//IETF//DTD HTML 2.0 Level 2//"
  , "-//IETF//DTD HTML 2.0 Strict Level 1//"
  , "-//IETF//DTD HTML 2.0 Strict Level 2//"
  , "-//IETF//DTD HTML 2.0 Strict//"
  , "-//IETF//DTD HTML 2.0//"
  , "-//IETF//DTD HTML 2.1E//"
  , "-//IETF//DTD HTML 3.0//"
  , "-//IETF//DTD HTML 3.2 Final//"
  , "-//IETF//DTD HTML 3.2//"
  , "-//IETF//DTD HTML 3//"
  , "-//IETF//DTD HTML Level 0//"
  , "-//IETF//DTD HTML Level 1//"
  , "-//IETF//DTD HTML Level 2//"
  , "-//IETF//DTD HTML Level 3//"
  , "-//IETF//DTD HTML Strict Level 0//"
  , "-//IETF//DTD HTML Strict Level 1//"
  , "-//IETF//DTD HTML Strict Level 2//"
  , "-//IETF//DTD HTML Strict Level 3//"
  , "-//IETF//DTD HTML Strict//"
  , "-//IETF//DTD HTML//"
  , "-//Metrius//DTD Metrius Presentational//"
  , "-//Microsoft//DTD Internet Explorer 2.0 HTML Strict//"
  , "-//Microsoft//DTD Internet Explorer 2.0 HTML//"
  , "-//Microsoft//DTD Internet Explorer 2.0 Tables//"
  , "-//Microsoft//DTD Internet Explorer 3.0 HTML Strict//"
  , "-//Microsoft//DTD Internet Explorer 3.0 HTML//"
  , "-//Microsoft//DTD Internet Explorer 3.0 Tables//"
  , "-//Netscape Comm. Corp.//DTD HTML//"
  , "-//Netscape Comm. Corp.//DTD Strict HTML//"
  , "-//O'Reilly and Associates//DTD HTML 2.0//"
  , "-//O'Reilly and Associates//DTD HTML Extended 1.0//"
  , "-//O'Reilly and Associates//DTD HTML Extended Relaxed 1.0//"
  , "-//SQ//DTD HTML 2.0 HoTMetaL + extensions//"
  , "-//SoftQuad Software//DTD HoTMetaL PRO 6.0::19990601::extensions to HTML 4.0//"
  , "-//SoftQuad//DTD HoTMetaL PRO 4.0::19971010::extensions to HTML 4.0//"
  , "-//Spyglass//DTD HTML 2.0 Extended//"
  , "-//Sun Microsystems Corp.//DTD HotJava HTML//"
  , "-//Sun Microsystems Corp.//DTD HotJava Strict HTML//"
  , "-//W3C//DTD HTML 3 1995-03-24//"
  , "-//W3C//DTD HTML 3.2 Draft//"
  , "-//W3C//DTD HTML 3.2 Final//"
  , "-//W3C//DTD HTML 3.2//"
  , "-//W3C//DTD HTML 3.2S Draft//"
  , "-//W3C//DTD HTML 4.0 Frameset//"
  , "-//W3C//DTD HTML 4.0 Transitional//"
  , "-//W3C//DTD HTML Experimental 19960712//"
  , "-//W3C//DTD HTML Experimental 970421//"
  , "-//W3C//DTD W3 HTML//"
  , "-//W3O//DTD W3 HTML 3.0//"
  , "-//WebTechs//DTD Mozilla HTML 2.0//"
  , "-//WebTechs//DTD Mozilla HTML//"
  ]

-- | Determines if the doctype token represents limited quirks mode.
tokenLimitedQuirks :: Token -> Bool
tokenLimitedQuirks TDoctype {..} =
  or
  [ anyPrefix tDoctypePublic
    [ "-//W3C//DTD XHTML 1.0 Frameset//"
    , "-//W3C//DTD XHTML 1.0 Transitional//"
    ]
  , and
    [ isJust tDoctypeSystem
    , anyPrefix tDoctypePublic
      [ "-//W3C//DTD HTML 4.01 Frameset//"
      , "-//W3C//DTD HTML 4.01 Transitional//"
      ]
    ]
  ]

-- | Determines if a DOCTYPE ID matches a value.
idMatch :: Maybe BS -> BS -> Bool
idMatch (Just x) y = bsLower x == bsLower y
idMatch Nothing y = False

-- | Determines if a text has any prefix from a list.
anyPrefix :: Maybe BS -> [BS] -> Bool
anyPrefix (Just x) ys = any (\y -> y `bsPrefixCI` x) ys
anyPrefix Nothing ys = False

-- | Handle the initial insertion mode.
doModeInitial :: Parser s -> Token -> ST s ()
doModeInitial p@Parser {..} t =
  case t of
    TChar {..} | chrWhitespace tCharData -> do
      pure ()
    TComment {} -> do
      insertDocComment p t
    TDoctype {} -> do
      doctypeTokenCheck p t
      doctypeMake p t >>= insertNewDocumentNode p
      iframe <- iframeSrcDoc p
      when (not iframe && tokenQuirks t) $ do
        modifyDOM p $ domQuirksSet DOMQuirksMode
      when (not iframe && tokenLimitedQuirks t) $ do
        modifyDOM p $ domQuirksSet DOMQuirksLimited
      setMode p ModeBeforeHtml
    _otherwise -> do
      whenM (notM $ iframeSrcDoc p) $ do
        parseError p (Just t) "initial unexpected token"
        modifyDOM p $ domQuirksSet DOMQuirksMode
      setMode p ModeBeforeHtml
      reprocess p t

-- | Handle the before html insertion mode.
doModeBeforeHtml :: Parser s -> Token -> ST s ()
doModeBeforeHtml p@Parser {..} t =
  case t of
    TDoctype {} ->
      parseError p (Just t) "before html doctype"
    TComment {} ->
      insertDocComment p t
    TChar {..} | chrWhitespace tCharData ->
      pure ()
    TStart { tStartName = "html" } -> do
      insertHtmlElement p t
      setMode p ModeBeforeHead
    TEnd { tEndName = x }
      | elem x [ "head", "body", "html", "br" ] -> do
          insertHtmlElementNamed p "html"
          setMode p ModeBeforeHead
          reprocess p t
    TEnd {} ->
      parseError p (Just t) "before html end tag"
    _otherwise -> do
      insertHtmlElementNamed p "html"
      setMode p ModeBeforeHead
      reprocess p t

-- | Handle the before head insertion mode.
doModeBeforeHead :: Parser s -> Token -> ST s ()
doModeBeforeHead p@Parser {..} t =
  case t of
    TChar {..} | chrWhitespace tCharData ->
      pure ()
    TComment {} ->
      insertComment p t
    TDoctype {} ->
      parseError p (Just t) "before head doctype"
    TStart { tStartName = "html" } ->
      doModeInBody p t
    TStart { tStartName = "head" } -> do
      insertHtmlElement p t
      saveHead p
      setMode p ModeInHead
    TEnd { tEndName = x }
      | elem x [ "head", "body", "html", "br" ] -> do
          insertHtmlElementNamed p "head"
          saveHead p
          setMode p ModeInHead
          reprocess p t
    TEnd {} ->
      parseError p (Just t) "before head end tag"
    _otherwise -> do
      insertHtmlElementNamed p "head"
      saveHead p
      setMode p ModeInHead
      reprocess p t

-- | Handles the in-head parser mode.
doModeInHead :: Parser s -> Token -> ST s ()
doModeInHead p@Parser {..} t =
  case t of
    TChar {..} | chrWhitespace tCharData ->
      insertChar p t
    TComment {} ->
      insertComment p t
    TDoctype {} ->
      warn "doctype"
    TStart { tStartName = "html" } ->
      doModeInBody p t
    TStart { tStartName = x }
      | elem x [ "base", "basefont", "bgsound", "link" ] -> do
          insertHtmlElement p t
          elementStackPop p
          selfClosingAcknowledge p
    TStart { tStartName = "meta" } -> do
      insertHtmlElement p t
      elementStackPop p
      selfClosingAcknowledge p
      -- The standard normally requires running the encoding determination
      -- algorithm here, but we should not need it since the content should
      -- be known to be UTF-16.
    TStart { tStartName = "title" } ->
      insertElementRCDATA p t
    TStart { tStartName = "noframes" } ->
      insertElementRAWTEXT p t
    TStart { tStartName = "style" } ->
      insertElementRAWTEXT p t
    TStart { tStartName = "noscript" } -> do
      insertHtmlElement p t
      setMode p ModeInHeadNoscript
    TStart { tStartName = "script" } -> do
      insertHtmlElement p t
      parserSetScriptData p
      saveMode p
      setMode p ModeText
    TEnd { tEndName = "head" } -> do
      elementStackPop p
      setMode p ModeAfterHead
    TEnd { tEndName = x }
      | elem x [ "body", "html", "br" ] -> do
          elementStackPop p
          setMode p ModeAfterHead
          reprocess p t
    TStart { tStartName = "template" } -> do
      insertHtmlElement p t
      activeFormatAddMarker p
      frameSetNotOK p
      setMode p ModeInTemplate
      templateModePush p ModeInTemplate
    TEnd { tEndName = x@"template" } -> do
      let a = domMakeTypeHTML x
      -- Make sure a template element is on the stack.
      elementStackHasTemplate p >>= \case
        False ->
          warn "template start tag missing"
        True -> do
          -- (1) Generate implied end tags thoroughly.
          generateImpliedEndTagsThoroughly p
          -- (2) Make sure template is the current node.
          unlessM (currentNodeHasType p a) $
            parseError p Nothing "template not current node"
          -- (3) Pop elements until a template has been popped.
          elementStackPopUntilType p a
          -- (4) Clear list of active formatting elements up to last marker.
          activeFormatClear p
          -- (5) Pop the current template insertion mode.
          templateModePop p
          -- (6) Reset the insertion mode appropriately.
          resetInsertionMode p
    TStart { tStartName = "head" } ->
      warn "head"
    TEnd {} ->
      warn "unexpected end tag"
    _otherwise -> do
      elementStackPop p
      setMode p ModeAfterHead
      reprocess p t
  where
    warn x = parseError p (Just t) $ "in head " <> x

-- | Handles the in head no script parser mode.
doModeInHeadNoscript :: Parser s -> Token -> ST s ()
doModeInHeadNoscript p@Parser {..} t =
  case t of
    TDoctype {} ->
      warn "doctype"
    TStart { tStartName = "html" } ->
      doModeInBody p t
    TEnd { tEndName = "noscript" } -> do
      elementStackPop p
      setMode p ModeInHead
    TChar {..} | chrWhitespace tCharData ->
      doModeInHead p t
    TComment {} ->
      doModeInHead p t
    TStart { tStartName = x }
      | elem x [ "basefont", "bgsound", "link",
                 "meta", "noframes", "style" ] ->
          doModeInHead p t
    TEnd { tEndName = "br" } -> do
      warn "br"
      elementStackPop p
      setMode p ModeInHead
      reprocess p t
    TStart { tStartName = "head" } ->
      warn "head"
    TStart { tStartName = "noscript" } ->
      warn "noscript"
    TEnd {} ->
      warn "end tag"
    _otherwise -> do
      warn "bad token"
      elementStackPop p
      setMode p ModeInHead
      reprocess p t
  where
    warn x = parseError p (Just t) $ "in head noscript " <> x

-- | Handles the after head parser mode.
doModeAfterHead :: Parser s -> Token -> ST s ()
doModeAfterHead p@Parser {..} t =
  case t of
    TChar {..} | chrWhitespace tCharData ->
      insertChar p t
    TComment {} ->
      insertComment p t
    TDoctype {} ->
      parseError p (Just t) "after head doctype"
    TStart { tStartName = "html" } ->
      doModeInBody p t
    TStart { tStartName = "body" } -> do
      insertHtmlElement p t
      frameSetNotOK p
      setMode p ModeInBody
    TStart { tStartName = "frameset" } -> do
      insertHtmlElement p t
      setMode p ModeInFrameset
    TStart { tStartName = x }
      | elem x
        [ "base"
        , "basefont"
        , "bgsound"
        , "link"
        , "meta"
        , "noframes"
        , "script"
        , "style"
        , "template"
        , "title"
        , "head"
        ] -> do
          parseError p (Just t) "after head bad start tag"
          getHeadID p >>= \case
            Just h -> do
              elementStackPush p h
              doModeInHead p t
              elementStackRemove p h
            Nothing -> do
              -- TODO: might need an error in this case.
              pure ()
    TEnd { tEndName = "template" } ->
      doModeInHead p t
    TEnd { tEndName = x }
      | elem x
        [ "body"
        , "html"
        , "br"
        ] -> do
          insertHtmlElementNamed p "body"
          setMode p ModeInBody
          reprocess p t
    TStart { tStartName = "head" } ->
      parseError p (Just t) "after head head"
    TEnd {} ->
      parseError p (Just t) "after head end tag"
    _otherwise -> do
      insertHtmlElementNamed p "body"
      setMode p ModeInBody
      reprocess p t

-- | Handles the in body parser mode.
doModeInBody :: Parser s -> Token -> ST s ()
doModeInBody p@Parser {..} t =
  case t of
    TChar {..} | chrWhitespace tCharData -> do
      activeFormatReconstruct p
      insertChar p t
    TChar {} -> do
      activeFormatReconstruct p
      insertChar p t
      frameSetNotOK p
    TComment {} ->
      insertComment p t
    TDoctype {} ->
      warn "doctype"
    TStart { tStartName = x@"html" } -> do
      warn x
      unlessM (elementStackHasTemplate p) $ do
        lastNodeID p >>= \case
          Just i -> do
            modifyDOM p $ domAttrMerge i $ Seq.fromList $
              map (\(TAttr n v s) -> DOMAttr n v s) $ tStartAttr t
          Nothing -> do
            -- TODO: consider an error for this case
            pure ()
    TStart { tStartName = x }
      | elem x
        [ "base"
        , "basefont"
        , "bgsound"
        , "link"
        , "meta"
        , "noframes"
        , "script"
        , "style"
        , "template"
        , "title"
        ] ->
          doModeInHead p t
    TEnd { tEndName = "template" } ->
      doModeInHead p t
    TStart { tStartName = x@"body" } -> do
      warn x
      unlessM (notM (elementStackHasBody p)
        ||^ liftA (==1) (elementStackSize p)
        ||^ (elementStackHasTemplate p)) $ do
          frameSetNotOK p
          y <- listToMaybe . drop 1 . reverse <$> elementStack p
          case y of
            Just i -> do
              modifyDOM p $ domAttrMerge i $ Seq.fromList $
                map (\(TAttr n v s) -> DOMAttr n v s) $ tStartAttr t
            Nothing -> do
              pure ()
    TStart { tStartName = x@"frameset" } -> do
      warn x
      unlessM (liftA (==1) (elementStackSize p)
        ||^ notM (elementStackHasBody p)
        ||^ notM (rref parserFrameSetOK)) $ do
          y <- listToMaybe . drop 1 . reverse <$> elementStackNodes p
          case y of
            Just n -> do
              modifyDOM p $ domRemoveChild (domNodeParent n) $ domNodeID n
              elementStackPopWhile p $ \n ->
                domNodeType n /= domMakeTypeHTML "html"
              insertHtmlElement p t
              setMode p ModeInFrameset
            Nothing -> do
              pure ()
    TEOF -> do
      n <- templateModeCount p
      if n > 0
      then doModeInTemplate p t
      else do
        whenM (elementStackAny p $ elementNameNotIn
          ["dd", "dt", "li", "menuitem", "optgroup",
           "option", "p", "rb", "rp", "rt", "rtc",
           "tbody", "td", "tfoot", "th", "thead", "tr",
           "body", "html"]) $
          warn "bad element on stack"
        parserSetDone p
    TEnd { tEndName = x@"body" } -> do
      let a = domMakeTypeHTML x
      elementInScope p a >>= \case
        False ->
          warn "no body element in scope"
        True -> do
          whenM (elementStackAny p $ elementNameNotIn
            ["dd", "dt", "li", "menuitem", "optgroup",
             "option", "p", "rb", "rp", "rt", "rtc",
             "tbody", "td", "tfoot", "th", "thead", "tr",
             "body", "html"]) $
            warn "bad element on stack"
          setMode p ModeAfterBody
    TEnd { tEndName = x@"html" } -> do
      let a = domMakeTypeHTML x
      elementInScope p a >>= \case
        False ->
          warn "no body element in scope"
        True -> do
          whenM (elementStackAny p $ elementNameNotIn
            ["dd", "dt", "li", "menuitem", "optgroup",
             "option", "p", "rb", "rp", "rt", "rtc",
             "tbody", "td", "tfoot", "th", "thead", "tr",
             "body", "html"]) $
            warn "bad element on stack"
          setMode p ModeAfterBody
          reprocess p t
    TStart { tStartName = x } | elem x
      ["address", "article", "aside", "blockquote",
       "center", "details", "dialog", "dir", "div", "dl",
       "fieldset", "figcaption", "figure", "footer", "header",
       "hgroup", "main", "nav", "ol", "p", "section",
       "summary", "ul"] -> do
      closeP
      insertHtmlElement p t
    TStart { tStartName = "menu" } -> do
      closeP
      popMenuitem
      insertHtmlElement p t
    TStart { tStartName = x }
      | elem x ["h1", "h2", "h3", "h4", "h5", "h6"] -> do
        closeP
        whenM (currentNodeHasTypeIn p $ domTypesHTML
          ["h1", "h2", "h3", "h4", "h5", "h6"]) $ do
          warn "bad header tag on stack"
          elementStackPop p
        insertHtmlElement p t
    TStart { tStartName = x } | elem x ["pre", "listing"] -> do
      closeP
      insertHtmlElement p t
      parserSkipNextLF p
      frameSetNotOK p
    TStart { tStartName = "form" } -> do
      (formNotNull p &&^ elementStackMissingTemplate p) >>= \case
        True ->
          warn "form without template"
        False -> do
          closeP
          insertHtmlElement p t
          whenM (elementStackMissingTemplate p) $ saveForm p
    TStart { tStartName = x@"li" } -> do
      let a = domMakeTypeHTML x
          s = Set.fromList $ domTypesHTML [ "address", "div", "p" ]
          f [] = pure ()
          f (y:ys)
            | y == a = do
                generateImpliedEndTagsExcept p x
                unlessM (currentNodeHasType p a) $
                  warn "current node is not li"
                elementStackPopUntilType p a
            | elementIsSpecial y && Set.notMember y s = pure ()
            | otherwise = f ys
      frameSetNotOK p
      elementStackTypes p >>= f
      closeP
      insertHtmlElement p t
    TStart { tStartName = x } | elem x ["dd", "dt"] -> do
      let dd = domMakeTypeHTML "dd"
          dt = domMakeTypeHTML "dt"
          s = Set.fromList $ domTypesHTML [ "address", "div", "p" ]
          f [] = pure ()
          f (y:ys)
            | y == dd || y == dt = do
                generateImpliedEndTagsExcept p $ domTypeName y
                unlessM (currentNodeHasType p y) $
                  warn "current node is not dd or dt"
                elementStackPopUntilType p y
            | elementIsSpecial y && Set.notMember y s = pure ()
            | otherwise = f ys
      frameSetNotOK p
      elementStackTypes p >>= f
      closeP
      insertHtmlElement p t
    TStart { tStartName = "plaintext" } -> do
      closeP
      insertHtmlElement p t
      parserSetPLAINTEXT p
    TStart { tStartName = x@"button" } -> do
      let a = domMakeTypeHTML x
      whenM (elementInScope p a) $ do
        warn "button element in scope"
        generateImpliedEndTags p
        elementStackPopUntilType p a
      activeFormatReconstruct p
      insertHtmlElement p t
      frameSetNotOK p
    TEnd { tEndName = x }
      | elem x ["address", "article", "aside", "blockquote", "button",
                "center", "details", "dialog", "dir", "div", "dl",
                "fieldset", "figcaption", "figure", "footer", "header",
                "hgroup", "listing", "main", "menu", "nav", "ol",
                "pre", "section", "summary", "ul"] -> do
          let a = domMakeTypeHTML x
          elementInScope p a >>= \case
            False ->
              warn "element not in scope"
            True -> do
              generateImpliedEndTags p
              unlessM (currentNodeHasType p a) $
                warn "current node wrong type"
              elementStackPopUntilType p a
    TEnd { tEndName = x@"form" } ->
      elementStackHasTemplate p >>= \case
        False -> do
          getFormID p >>= \case
            Nothing ->
              warn "form not defined"
            Just n -> do
              a <- fromJust <$> getFormType p
              setFormID p Nothing
              elementInScope p a >>= \case
                False ->
                  warn "form not in scope"
                True -> do
                  generateImpliedEndTags p
                  unlessM (liftA (==(Just n)) (currentNodeID p)) $
                    warn "current node is wrong node"
                  elementStackRemove p n
        True -> do
          let a = domMakeTypeHTML x
          elementInScope p a >>= \case
            False ->
              warn "form not in scope"
            True -> do
              generateImpliedEndTags p
              unlessM (currentNodeHasType p a) $
                warn "current node is not a form"
              elementStackPopUntilType p a
    TEnd { tEndName = x@"p" } -> do
      let a = domMakeTypeHTML x
      unlessM (elementInButtonScope p a) $ do
        warn "no p in button scope"
        insertHtmlElementNamed p x
      closeElementP p
    TEnd { tEndName = x@"li" } -> do
      let a = domMakeTypeHTML x
      elementInListScope p a >>= \case
        False ->
          warn "no li in list scope"
        True -> do
          generateImpliedEndTagsExcept p x
          unlessM (currentNodeHasType p a) $
            warn "current node not an li element"
          elementStackPopUntilType p a
    TEnd { tEndName = x } | elem x ["dd", "dt"] -> do
      let a = domMakeTypeHTML x
      elementInListScope p a >>= \case
        False ->
          warn "no dd or dt in list scope"
        True -> do
          generateImpliedEndTagsExcept p x
          unlessM (currentNodeHasType p a) $
            warn "current not is not a dd or dt"
          elementStackPopUntilType p a
    TEnd { tEndName = x }
      | elem x ["h1", "h2", "h3", "h4", "h5", "h6"] -> do
          let h = domTypesHTML ["h1", "h2", "h3", "h4", "h5", "h6"]
          anyM (elementInScope p) h >>= \case
            False ->
              warn "header element not in scope"
            True -> do
              generateImpliedEndTags p
              unlessM (currentNodeHasType p $ domMakeTypeHTML x) $
                warn "current node not a header type"
              elementStackPopUntilTypeIn p h
    TEnd { tEndName = "sarcasm" } ->
      doAnyOtherEndTag
    TStart { tStartName = x@"a" } -> do
      let a = domMakeTypeHTML x
      activeFormatFindTag p x >>= \case
        Nothing -> pure ()
        Just (ParserFormatElement i _) -> do
          warn "active format already has anchor"
          runAA x
          elementStackRemove p i
          activeFormatRemove p i
      activeFormatReconstruct p
      insertHtmlElement p t
      activeFormatAddCurrentNode p t
    TStart { tStartName = x } | elem x
      ["b", "big", "code", "em", "font", "i", "s",
       "small", "strike", "strong", "tt", "u"] -> do
      activeFormatReconstruct p
      insertHtmlElement p t
      activeFormatAddCurrentNode p t
    TStart { tStartName = x@"nobr" } -> do
      let a = domMakeTypeHTML x
      activeFormatReconstruct p
      whenM (elementInScope p a) $ do
        warn "nobr tag when nobr element already in scope"
        runAA x
        activeFormatReconstruct p
      insertHtmlElement p t
      activeFormatAddCurrentNode p t
    TEnd { tEndName = x } | elem x
      ["a", "b", "big", "code", "em", "font", "i", "nobr",
       "s", "small", "strike", "strong", "tt", "u"] ->
      runAA x
    TStart { tStartName = x } | elem x
      ["applet", "marquee", "object"] -> do
      activeFormatReconstruct p
      insertHtmlElement p t
      activeFormatAddMarker p
      frameSetNotOK p
    TEnd { tEndName = x } | elem x
      ["applet", "marquee", "object"] -> do
      let a = domMakeTypeHTML x
      elementInScope p a >>= \case
        False ->
          warn "element scope missing"
        True -> do
          generateImpliedEndTags p
          unlessM (currentNodeHasType p a) $
            warn "current node is wring type"
          elementStackPopUntilType p a
          activeFormatClear p
    TStart { tStartName = "table" } -> do
      q <- domQuirksGet <$> getDOM p
      when (q /= DOMQuirksMode) closeP
      insertHtmlElement p t
      frameSetNotOK p
      setMode p ModeInTable
    TEnd { tEndName = "br" } -> do
      warn "br end tag"
      activeFormatReconstruct p
      insertHtmlElement p $ TStart "br" False []
      elementStackPop p
      selfClosingAcknowledge p
      frameSetNotOK p
    TStart { tStartName = x } | elem x
      ["area", "br", "embed", "img", "keygen", "wbr"] -> do
      activeFormatReconstruct p
      insertHtmlElement p t
      elementStackPop p
      selfClosingAcknowledge p
      frameSetNotOK p
    TStart { tStartName = "input" } -> do
      activeFormatReconstruct p
      insertHtmlElement p t
      elementStackPop p
      selfClosingAcknowledge p
      case tokenGetAttrVal "type" t of
        Just v -> when (bsLower v /= "hidden") $ frameSetNotOK p
        Nothing -> frameSetNotOK p
    TStart { tStartName = x } | elem x
      ["param", "source", "track"] -> do
      insertHtmlElement p t
      elementStackPop p
      selfClosingAcknowledge p
    TStart { tStartName = "hr" } -> do
      closeP
      popMenuitem
      insertHtmlElement p t
      elementStackPop p
      selfClosingAcknowledge p
      frameSetNotOK p
    TStart { tStartName = "image" } -> do
      warn "image"
      let t' = t { tStartName = "img" }
      reprocess p t'
    TStart { tStartName = "textarea" } -> do
      insertHtmlElement p t
      parserSkipNextLF p
      parserSetRCDATA p
      saveMode p
      frameSetNotOK p
      setMode p ModeText
    TStart { tStartName = "xmp" } -> do
      closeP
      activeFormatReconstruct p
      frameSetNotOK p
      insertElementRAWTEXT p t
    TStart { tStartName = "iframe" } -> do
      frameSetNotOK p
      insertElementRAWTEXT p t
    TStart { tStartName = "noembed" } ->
      insertElementRAWTEXT p t
    TStart { tStartName = "select" } -> do
      activeFormatReconstruct p
      insertHtmlElement p t
      frameSetNotOK p
      s <- pure $ Set.fromList
        [ ModeInTable, ModeInCaption, ModeInTableBody,
          ModeInRow, ModeInCell ]
      rref parserInsertionMode >>= \x -> setMode p $
        if Set.member x s
           then ModeInSelectInTable
           else ModeInSelect
    TStart { tStartName = x } | elem x
      ["optgroup", "option"] -> do
      elementStackPopIf p $ elementName "option"
      activeFormatReconstruct p
      insertHtmlElement p t
    TStart { tStartName = "menuitem" } -> do
      popMenuitem
      insertHtmlElement p t
    TStart { tStartName = x } | elem x ["rb", "rtc"] -> do
      let a = domMakeTypeHTML "ruby"
      whenM (elementInScope p a) $ do
        generateImpliedEndTags p
        unlessM (currentNodeHasType p a) $
          warn "ruby element not in scope"
      insertHtmlElement p t
    TStart { tStartName = x } | elem x ["rp", "rt"] -> do
      let a = domMakeTypeHTML "ruby"
          b = domMakeTypeHTML "rtc"
      whenM (elementInScope p a) $ do
        generateImpliedEndTagsExcept p "rtc"
        unlessM (currentNodeHasType p a ||^ currentNodeHasType p b) $
          warn "ruby or rtc element not in scope"
      insertHtmlElement p t
    TStart { tStartName = "math" } -> do
      activeFormatReconstruct p
      insertMathMLElement p . adjustAttrForeign . adjustAttrMathML $ t
      when (tStartClosed t) $ do
        elementStackPop p
        selfClosingAcknowledge p
    TStart { tStartName = "svg" } -> do
      activeFormatReconstruct p
      insertSvgElement p . adjustAttrForeign . adjustAttrSVG $ t
      when (tStartClosed t) $ do
        elementStackPop p
        selfClosingAcknowledge p
    TStart { tStartName = x } | elem x
      ["caption", "col", "colgroup", "frame", "head",
       "tbody", "td", "tfoot", "th", "thead", "tr"] ->
      warn "bad start token"
    TStart {} -> do
      activeFormatReconstruct p
      insertHtmlElement p t
    TEnd {} ->
      doAnyOtherEndTag
  where
    closeP = do
      let a = domMakeTypeHTML "p"
      whenM (elementInButtonScope p a) $ closeElementP p
    popMenuitem =
      elementStackPopIf p $ elementName "menuitem"
    runAA =
      flip (adoptionAgencyRun p) doAnyOtherEndTag
    doAnyOtherEndTag =
      elementStackTypes p >>= f
      where
        n = tEndName t
        a = domMakeTypeHTML n
        f [] = pure ()
        f (x:xs)
          | x == a = do
              generateImpliedEndTagsExcept p n
              unlessM (currentNodeHasType p x) $
                warn "current node has wrong type"
              elementStackPop p
          | elementIsSpecial x = do
              warn "special element in stack"
          | otherwise = f xs
    warn x =
      parseError p (Just t) $ "in body " <> x

-- | Handle the text insertion mode.
doModeText :: Parser s -> Token -> ST s ()
doModeText p@Parser {..} t =
  case t of
    TChar {} ->
      insertChar p t
    TEOF -> do
      parseError p (Just t) "text eof"
      elementStackPop p
      restoreMode p
      reprocess p t
    TEnd { tEndName = "script" } -> do
      elementStackPop p
      restoreMode p
      -- The standard explains how to execute the script
      -- at this point, but we are just parsing.
    _otherwise -> do
      elementStackPop p
      restoreMode p

-- | Handle the in-table insertion mode.
doModeInTable :: Parser s -> Token -> ST s ()
doModeInTable p@Parser {..} t =
  case t of
    TChar {} -> do
      a <- currentNodeHasTypeIn p $ domTypesHTML
        ["table", "tbody", "tfoot", "thead", "tr"]
      if a
      then do
        pendingTableCharInit p
        saveMode p
        setMode p ModeInTableText
        reprocess p t
      else do
        -- Because of the way the check is implmented, perform the
        -- steps for anything else here.
        anythingElse
    TComment {} ->
      insertComment p t
    TDoctype {} ->
      warn "doctype"
    TStart { tStartName = "caption" } -> do
      clearToTableContext
      activeFormatAddMarker p
      insertHtmlElement p t
      setMode p ModeInCaption
    TStart { tStartName = "colgroup" } -> do
      clearToTableContext
      insertHtmlElement p t
      setMode p ModeInColumnGroup
    TStart { tStartName = x@"col" } -> do
      clearToTableContext
      insertHtmlElementNamed p x
      setMode p ModeInColumnGroup
      reprocess p t
    TStart { tStartName = x } | elem x
      ["tbody", "tfoot", "thead"] -> do
      clearToTableContext
      insertHtmlElement p t
      setMode p ModeInTableBody
    TStart { tStartName = x } | elem x
      ["td", "th", "tr"] -> do
      clearToTableContext
      insertHtmlElementNamed p "tbody"
      setMode p ModeInTableBody
      reprocess p t
    TStart { tStartName = x@"table" } -> do
      warn "table start tag"
      let a = domMakeTypeHTML x
      unlessM (elementInTableScope p a) $ do
        elementStackPopUntilType p a
        resetInsertionMode p
        reprocess p t
    TEnd { tEndName = x@"table" } -> do
      let a = domMakeTypeHTML x
      elementInTableScope p a >>= \case
        False ->
          warn "no table in scope"
        True -> do
          elementStackPopUntilType p a
          resetInsertionMode p
    TStart { tStartName = x } | elem x
      ["body", "caption", "col", "colgroup", "html",
       "tbody", "td", "tfoot", "th", "thead", "tr"] ->
      warn "unexpected start tag"
    TStart { tStartName = x } | elem x
     ["style", "script", "template"] ->
      doModeInHead p t
    TEnd { tEndName = "template" } ->
      doModeInHead p t
    TStart { tStartName = "input" } -> do
      if case tokenGetAttr "type" t of
        Nothing -> True
        Just a -> bsLower (tAttrName a) /= "hidden"
      then anythingElse
      else do
        warn "hidden input"
        insertHtmlElement p t
        elementStackPop p
        selfClosingAcknowledge p
    TStart { tStartName = "form" } -> do
      warn "form start tag"
      unlessM (elementStackHasTemplate p ||^ formNotNull p) $ do
        insertHtmlElement p t
        saveForm p
        elementStackPop p
    TEOF ->
      doModeInBody p t
    _otherwise ->
      anythingElse
  where
    clearToTableContext =
      elementStackPopWhile p $ \x -> not $ elem (domNodeType x) $
        domTypesHTML ["table", "template", "html"]
    anythingElse = do
      warn "unexpected token"
      fosterParentingSet p
      doModeInBody p t
      fosterParentingClear p
    warn x =
      parseError p (Just t) $ "in table " <> x

-- | Handle the in-table-text insertion mode.
doModeInTableText :: Parser s -> Token -> ST s ()
doModeInTableText p@Parser {..} t =
  case t of
    TChar {} ->
      pendingTableCharAppend p t
    _otherwise -> do
      a <- pendingTableChars p
      if any (not . chrWhitespace . tCharData) a
      then do
        warn "unexpected character"
        fosterParentingSet p
        mapM_ (doModeInBody p) a
        fosterParentingClear p
      else do
        mapM_ (insertChar p) a
      restoreMode p
      reprocess p t
  where
    warn x =
      parseError p (Just t) $ "in table text " <> x

-- | Handle the in-caption insertion mode.
doModeInCaption :: Parser s -> Token -> ST s ()
doModeInCaption p@Parser {..} t =
  case t of
    TEnd { tEndName = x@"caption" } ->
      processCaption
    TStart { tStartName = x } | elem x
      ["caption", "col", "colgroup",
       "tbody", "td", "tfoot", "th", "thead", "tr"] -> do
      processCaption
      reprocess p t
    TEnd { tEndName = "table" } -> do
      processCaption
      reprocess p t
    TEnd { tEndName = x } | elem x
      ["body", "col", "colgroup", "html",
       "tbody", "td", "tfoot", "th", "thead", "tr"] ->
      warn "unexpected end tag"
    _otherwise ->
      doModeInBody p t
  where
    processCaption = do
      let a = domMakeTypeHTML "caption"
      elementInTableScope p a >>= \case
        False ->
          warn "no caption in table scope"
        True -> do
          generateImpliedEndTags p
          unlessM (currentNodeHasType p a) $
            warn "current node is not a caption"
          elementStackPopUntilType p a
          activeFormatClear p
          setMode p ModeInTable
    warn x =
      parseError p (Just t) $ "in caption " <> x

-- | Handle the in-column-group insertion mode.
doModeInColumnGroup :: Parser s -> Token -> ST s ()
doModeInColumnGroup p@Parser {..} t =
  case t of
    TChar {..} | chrWhitespace tCharData ->
      insertChar p t
    TComment {} ->
      insertComment p t
    TDoctype {} ->
      warn "doctype"
    TStart { tStartName = "html" } ->
      doModeInBody p t
    TStart { tStartName = "col" } -> do
      insertHtmlElement p t
      elementStackPop p
      selfClosingAcknowledge p
    TEnd { tEndName = x@"colgroup" } -> do
      let a = domMakeTypeHTML x
      currentNodeHasType p a >>= \case
        False ->
          warn "current node not colgroup end"
        True -> do
          elementStackPop p
          setMode p ModeInTable
    TEnd { tEndName = "col" } ->
      warn "col end tag"
    TStart { tStartName = "template" } ->
      doModeInHead p t
    TEnd { tEndName = "template" } ->
      doModeInHead p t
    TEOF ->
      doModeInBody p t
    _otherwise -> do
      let a = domMakeTypeHTML "colgroup"
      currentNodeHasType p a >>= \case
        False ->
          warn "current node not colgroup end"
        True -> do
          elementStackPop p
          setMode p ModeInTable
          reprocess p t
  where
    warn x =
      parseError p (Just t) $ "in column group " <> x

-- | Handle the in-table-body insertion mode.
doModeInTableBody :: Parser s -> Token -> ST s ()
doModeInTableBody p@Parser {..} t =
  case t of
    TStart { tStartName = "tr" } -> do
      clearToTableBodyContext
      insertHtmlElement p t
      setMode p ModeInRow
    TStart { tStartName = x } | elem x ["th", "td"] -> do
      warn "th or td missing tr"
      clearToTableBodyContext
      insertHtmlElementNamed p "tr"
      setMode p ModeInRow
      reprocess p t
    TEnd { tEndName = x } | elem x
      ["tbody", "tfoot", "thead"] -> do
      let a = domMakeTypeHTML x
      elementInTableScope p a >>= \case
        False ->
          warn "element not in table scope"
        True -> do
          clearToTableBodyContext
          elementStackPop p
          setMode p ModeInTable
    TStart { tStartName = x } | elem x
      ["caption", "col", "colgroup", "tbody", "tfoot", "thead"] ->
      processElements
    TEnd { tEndName = "table" } ->
      processElements
    TEnd { tEndName = x } | elem x
      ["body", "caption", "col", "colgroup", "html", "td", "th", "tr"] ->
      warn "unexpected end tag"
    _otherwise ->
      doModeInTable p t
  where
    processElements = do
      anyM (elementInTableScope p . domMakeTypeHTML)
        ["tbody", "tfoot", "thead"] >>= \case
        False ->
          warn "element not in table scope"
        True -> do
          clearToTableBodyContext
          elementStackPop p
          setMode p ModeInTable
          reprocess p t
    clearToTableBodyContext =
      elementStackPopWhile p $ \x -> not $ elem (domNodeType x) $
        domTypesHTML ["tbody", "tfoot", "thead", "template", "html"]
    warn x =
      parseError p (Just t) $ "in table body " <> x

-- | Handle the in-row insertion mode.
doModeInRow :: Parser s -> Token -> ST s ()
doModeInRow p@Parser {..} t =
  case t of
    TStart { tStartName = x } | elem x ["th", "td"] -> do
      clearToTableRowContext
      insertHtmlElement p t
      setMode p ModeInCell
      activeFormatAddMarker p
    TEnd { tEndName = "tr" } ->
      processTr
    TStart { tStartName = x } | elem x
      ["caption", "col", "colgroup", "tbody", "tfoot", "thead", "tr"] -> do
      processTr
      reprocess p t
    TEnd { tEndName = "table" } -> do
      processTr
      reprocess p t
    TEnd { tEndName = x } | elem x
      ["tbody", "tfoot", "thead"] -> do
      let a = domMakeTypeHTML x
          b = domMakeTypeHTML "tr"
      elementInTableScope p a >>= \case
        False ->
          warn "element not in table scope"
        True ->
          whenM (elementInTableScope p b) $ do
            clearToTableRowContext
            elementStackPop p
            setMode p ModeInTableBody
            reprocess p t
    TEnd { tEndName = x } | elem x
      ["body", "caption", "col", "colgroup", "html", "td", "th"] ->
      warn "unexpected end tag"
    _otherwise ->
      doModeInTable p t
  where
    processTr = do
      let a = domMakeTypeHTML "tr"
      elementInTableScope p a >>= \case
        False ->
          warn "element not in table scope"
        True -> do
          clearToTableRowContext
          elementStackPop p
          setMode p ModeInTableBody
    clearToTableRowContext =
      elementStackPopWhile p $ \x -> not $ elem (domNodeType x) $
        domTypesHTML ["tr", "template", "html"]
    warn x =
      parseError p (Just t) $ "in row " <> x

-- | Handle the in-cell insertion mode.
doModeInCell :: Parser s -> Token -> ST s ()
doModeInCell p@Parser {..} t =
  case t of
    TEnd { tEndName = x } | elem x ["td", "th"] -> do
      let a = domMakeTypeHTML x
      elementInTableScope p a >>= \case
        False ->
          warn "element not in table scope"
        True -> do
          generateImpliedEndTags p
          unlessM (currentNodeHasType p a) $
            warn $ "current node not " <> x
          elementStackPopUntilType p a
          activeFormatClear p
          setMode p ModeInRow
    TStart { tStartName = x } | elem x
      ["caption", "col", "colgroup",
       "tbody", "td", "tfoot", "th", "thead", "tr"] -> do
      anyM (elementInTableScope p . domMakeTypeHTML) ["td", "th"] >>= \case
        False ->
          warn "td or th not in table scope"
        True -> do
          closeCell
          reprocess p t
    TEnd { tEndName = x } | elem x
      ["body", "caption", "col", "colgroup", "html"] ->
      warn "unexpected end tag"
    TEnd { tEndName = x } | elem x
      ["table", "tbody", "tfoot", "thead", "tr"] -> do
      let a = domMakeTypeHTML x
      elementInTableScope p a >>= \case
        False ->
          warn "element not in table scope"
        True -> do
          closeCell
          reprocess p t
    _otherwise ->
      doModeInBody p t
  where
    closeCell = do
      let a = domTypesHTML ["td", "th"]
      generateImpliedEndTags p
      unlessM (currentNodeHasTypeIn p a) $
        warn "current node is not td or th"
      elementStackPopUntilTypeIn p a
      activeFormatClear p
      setMode p ModeInRow
    warn x =
      parseError p (Just t) $ "in cell " <> x

-- | Handle the in-select insertion mode.
doModeInSelect :: Parser s -> Token -> ST s ()
doModeInSelect p@Parser {..} t =
  case t of
    TChar {} ->
      insertChar p t
    TComment {} ->
      insertComment p t
    TDoctype {} ->
      warn "doctype"
    TStart { tStartName = "html" } ->
      doModeInBody p t
    TStart { tStartName = x@"option" } -> do
      elementStackPopIf p $ elementName x
      insertHtmlElement p t
    TStart { tStartName = x@"optgroup" } -> do
      elementStackPopIf p $ elementName "option"
      elementStackPopIf p $ elementName x
      insertHtmlElement p t
    TEnd { tEndName = x@"optgroup" } -> do
      let a = domMakeTypeHTML "option"
          b = domMakeTypeHTML x
      y <- take 2 <$> elementStackTypes p
      when (y == [a,b]) $ elementStackPop p
      currentNodeHasType p b >>= \case
        False ->
          warn $ "current node not " <> x
        True ->
          elementStackPop p
    TEnd { tEndName = x@"option" } -> do
      currentNodeHasType p (domMakeTypeHTML x) >>= \case
        False ->
          warn $ "current node not " <> x
        True ->
          elementStackPop p
    TEnd { tEndName = x@"select" } -> do
      let a = domMakeTypeHTML x
      elementInSelectScope p a >>= \case
        False ->
          warn "no select in select scope"
        True -> do
          elementStackPopUntilType p a
          resetInsertionMode p
    TStart { tStartName = x@"select" } -> do
      warn "unexpected start tag"
      let a = domMakeTypeHTML x
      whenM (elementInSelectScope p a) $ do
        elementStackPopUntilType p a
        resetInsertionMode p
    TStart { tStartName = x } | elem x
      ["input", "keygen", "textarea"] -> do
      warn "unexpected start tag"
      let a = domMakeTypeHTML x
      whenM (elementInSelectScope p a) $ do
        elementStackPopUntilType p a
        resetInsertionMode p
        reprocess p t
    TStart { tStartName = x } | elem x
     ["script", "template"] ->
      doModeInHead p t
    TEnd { tEndName = "template" } ->
      doModeInHead p t
    TEOF ->
      doModeInBody p t
    _otherwise ->
      warn "unexpected token"
  where
    warn x =
      parseError p (Just t) $ "in select " <> x

-- | Handle the in-select-in-table insertion mode.
doModeInSelectInTable :: Parser s -> Token -> ST s ()
doModeInSelectInTable p@Parser {..} t =
  case t of
    TStart { tStartName = x } | elem x
      ["caption", "table", "tbody", "tfoot",
       "thead", "tr", "td", "th"] -> do
      warn "unexpected start tag"
      elementStackPopUntilType p $ domMakeTypeHTML "select"
      resetInsertionMode p
      reprocess p t
    TEnd { tEndName = x } | elem x
      ["caption", "table", "tbody", "tfoot",
       "thead", "tr", "td", "th"] -> do
      warn "unexpected end tag"
      whenM (elementInTableScope p $ domMakeTypeHTML x) $ do
        elementStackPopUntilType p $ domMakeTypeHTML "select"
        resetInsertionMode p
        reprocess p t
    _otherwise ->
      doModeInSelect p t
  where
    warn x =
      parseError p (Just t) $ "in select in table " <> x

-- | Handle the in-template insertion mode.
doModeInTemplate :: Parser s -> Token -> ST s ()
doModeInTemplate p@Parser {..} t =
  case t of
    TChar {} ->
      doModeInBody p t
    TComment {} ->
      doModeInBody p t
    TDoctype {} ->
      doModeInBody p t
    TStart { tStartName = x } | elem x
      ["base", "basefont", "bgsound", "link", "meta",
       "noframes", "script", "style", "template", "title"] ->
      doModeInHead p t
    TEnd { tEndName = "template" } ->
      doModeInHead p t
    TStart { tStartName = x } | elem x
      ["caption", "col", "tbody", "tfoot", "thead"] -> do
      templateModePop p
      templateModePush p ModeInTable
      setMode p ModeInTable
      reprocess p t
    TStart { tStartName = "col" } -> do
      templateModePop p
      templateModePush p ModeInColumnGroup
      setMode p ModeInColumnGroup
      reprocess p t
    TStart { tStartName = "tr" } -> do
      templateModePop p
      templateModePush p ModeInTableBody
      setMode p ModeInTableBody
      reprocess p t
    TStart { tStartName = x } | elem x ["td", "th"] -> do
      templateModePop p
      templateModePush p ModeInRow
      setMode p ModeInRow
      reprocess p t
    TStart {} -> do
      templateModePop p
      templateModePush p ModeInBody
      setMode p ModeInBody
      reprocess p t
    TEnd {} ->
      warn "unexpected end tag"
    TEOF ->
      elementStackMissingTemplate p >>= \case
        True ->
          parserSetDone p
        False -> do
          warn "template on stack"
          activeFormatClear p
          templateModePop p
          resetInsertionMode p
          reprocess p t
  where
    warn x =
      parseError p (Just t) $ "in template " <> x

-- | Handle the after-body insertion mode.
doModeAfterBody :: Parser s -> Token -> ST s ()
doModeAfterBody p@Parser {..} t =
  case t of
    TChar {..} | chrWhitespace tCharData ->
      doModeInBody p t
    TComment {} -> do
      x <- domPos . fromJust <$> lastNodeID p
      commentMake p t >>= void . insertNewNode p x
    TDoctype {} ->
      warn "doctype"
    TStart { tStartName = "html" } ->
      doModeInBody p t
    TEnd { tEndName = "html" } ->
      rref parserFragmentMode >>= \case
        True ->
          warn "html end tag"
        False ->
          setMode p ModeAfterAfterBody
    TEOF ->
      parserSetDone p
    _otherwise -> do
      warn "unexpected token"
      setMode p ModeInBody
      reprocess p t
  where
    warn x =
      parseError p (Just t) $ "after body " <> x

-- | Handle the in-frameset insertion mode.
doModeInFrameset :: Parser s -> Token -> ST s ()
doModeInFrameset p@Parser {..} t =
  case t of
    TChar {..} | chrWhitespace tCharData ->
      insertChar p t
    TComment {} ->
      insertComment p t
    TDoctype {} ->
      warn "doctype"
    TStart { tStartName = "html" } ->
      doModeInBody p t
    TStart { tStartName = "frameset" } ->
      insertHtmlElement p t
    TEnd { tEndName = "frameset" } -> do
      currentNodeHasHTMLType p "html" >>= \case
        True ->
          warn "current node is html"
        False -> do
          elementStackPop p
          whenM (notM (rref parserFragmentMode) &&^
            notM (currentNodeHasHTMLType p "frameset")) $
            setMode p ModeAfterFrameset
    TStart { tStartName = "frame" } -> do
      insertHtmlElement p t
      elementStackPop p
      selfClosingAcknowledge p
    TStart { tStartName = "noframes" } ->
      doModeInHead p t
    TEOF -> do
      unlessM (currentNodeHasHTMLType p "html") $
        warn "current node is not html"
      parserSetDone p
    _ ->
      warn "unexpected token"
  where
    warn x =
      parseError p (Just t) $ "in frameset " <> x

-- | Handle the after-frameset insertion mode.
doModeAfterFrameset :: Parser s -> Token -> ST s ()
doModeAfterFrameset p@Parser {..} t =
  case t of
    TChar {..} | chrWhitespace tCharData ->
      insertChar p t
    TComment {} ->
      insertComment p t
    TDoctype {} ->
      warn "doctype"
    TStart { tStartName = "html" } ->
      doModeInBody p t
    TEnd { tEndName = "html" } ->
      setMode p ModeAfterAfterFrameset
    TStart { tStartName = "noframes" } ->
      doModeInHead p t
    TEOF ->
      parserSetDone p
    _ ->
      warn "unexpected token"
    where
      warn x =
        parseError p (Just t) $ "after frameset " <> x

-- | Handle the after-after-body insertion mode.
doModeAfterAfterBody :: Parser s -> Token -> ST s ()
doModeAfterAfterBody p@Parser {..} t =
  case t of
    TComment {} ->
      insertDocComment p t
    TDoctype {} ->
      doModeInBody p t
    TChar {..} | chrWhitespace tCharData ->
      doModeInBody p t
    TStart { tStartName = "html" } ->
      doModeInBody p t
    TEOF ->
      parserSetDone p
    _otherwise -> do
      warn "unexpected token"
      setMode p ModeInBody
      reprocess p t
    where
      warn x =
        parseError p (Just t) $ "after after body " <> x

-- | Handle the after-after-frameset insertion mode.
doModeAfterAfterFrameset :: Parser s -> Token -> ST s ()
doModeAfterAfterFrameset p@Parser {..} t =
  case t of
    TComment {} ->
      insertDocComment p t
    TDoctype {} ->
      doModeInBody p t
    TChar {..} | chrWhitespace tCharData ->
      doModeInBody p t
    TStart { tStartName = "html" } ->
      doModeInBody p t
    TEOF ->
      parserSetDone p
    TStart { tStartName = "noframes" } ->
      doModeInHead p t
    _otherwise -> do
      warn "unexpected token"
    where
      warn x =
        parseError p (Just t) $ "after after frameset " <> x

-- | Handle foreign content.
doForeignContent :: Parser s -> Token -> ST s ()
doForeignContent p@Parser {..} t =
  case t of
    TChar {..} | chrWhitespace tCharData ->
      insertChar p t
    TChar {} -> do
      insertChar p t
      frameSetNotOK p
    TComment {} ->
      insertComment p t
    TDoctype {} ->
      warn "doctype"
    TStart { tStartName = x } | elem x
      ["b", "big", "blockquote", "body", "br", "center",
       "code", "dd", "div", "dl", "dt", "em", "embed",
       "h1", "h2", "h3", "h4", "h5", "h6", "head",
       "hr", "i", "img", "li", "listing", "menu",
       "meta", "nobr", "ol", "p", "pre", "ruby", "s",
       "small", "span", "strong", "strike", "sub",
       "sup", "table", "tt", "u", "ul", "var"]
      || x == "font" &&
         any (flip tokenHasAttr t) ["color","face","size"] -> do
      warn "unexpected start tag"
      rref parserFragmentMode >>= \case
        True ->
          anyOtherStartTag
        False -> do
          elementStackPop p
          elementStackPopWhile p $ \n ->
            not (isMathMLIntegrationPoint n
                 || isHtmlIntgrationPoint n
                 || domNodeIsHTML n)
          reprocess p t
    TStart {} ->
      anyOtherStartTag
    TEnd {} -> do
      let s = "script"
          a = domMakeTypeSVG s
          n = tEndName t
      svg <- maybe False ((==) a . domNodeType) <$> currentNode p
      if n == s && svg
      then doScriptEndTag
      else do
        node <- fromJust <$> currentNode p
        let h = bsLower . domNodeElementName
            nodeName = h node
        when (nodeName /= n) $
          warn $
            "bad end tag in foreign content ("
            <> nodeName <> " /= " <> bcPack (show n) <> ")"
        let f (x:[]) = pure ()
            f (x:y:ys)
              | h x == n =
                  elementStackPopUntilID p $ domNodeID node
              | domNodeIsHTML y =
                  doHtmlContent p t
              | otherwise =
                  f (y:ys)
        elementStackNodes p >>= f
  where
    anyOtherStartTag = do
      (t', n) <- adjustedCurrentNode p >>= \case
        Just a
          | domNodeIsMathML a ->
              pure ( adjustAttrMathML t
                   , domNodeElementNamespace a
                   )
          | domNodeIsSVG a ->
              pure ( adjustElemSVG $ adjustAttrSVG t
                   , domNodeElementNamespace a
                   )
        Nothing ->
          pure (t, HTMLNamespaceHTML)
      insertForeignElement p n $ adjustAttrForeign t'
      when (tStartClosed t) $ do
        svg <- maybe False domNodeIsSVG <$> currentNode p
        if tStartName t == "script" && svg
        then do
          selfClosingAcknowledge p
          doScriptEndTag
        else do
          elementStackPop p
          selfClosingAcknowledge p
    doScriptEndTag = do
      elementStackPop p
    warn x =
      parseError p (Just t) $ "foreign content " <> x
