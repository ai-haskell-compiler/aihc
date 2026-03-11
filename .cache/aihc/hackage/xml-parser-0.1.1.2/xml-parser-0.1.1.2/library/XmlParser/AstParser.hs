module XmlParser.AstParser
  ( -- * Execution
    parseElement,
    renderElementError,
    ElementError (..),
    NodeType (..),

    -- * Parsers by context

    -- ** Element
    Element,
    elementName,
    elementNameIs,
    children,
    childrenByName,
    attributesByName,
    astElement,

    -- ** Nodes
    Nodes,
    elementNode,
    contentNode,

    -- ** ByName
    ByName,
    byName,

    -- ** Content
    Content,
    textContent,
    narrowedContent,
    refinedContent,
    enumContent,
    attoparsedContent,
    qNameContent,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Text.XML as Xml
import TextBuilder (TextBuilder)
import qualified TextBuilder
import qualified XmlParser.Attoparsec as Attoparsec
import qualified XmlParser.ElementDestructionState as ElementDestructionState
import qualified XmlParser.NameMap as NameMap
import qualified XmlParser.NamespaceRegistry as NamespaceRegistry
import qualified XmlParser.NodeConsumerState as NodeConsumerState
import XmlParser.Prelude

-- |
-- Parse an \"xml-conduit\" element AST.
parseElement :: Element a -> Xml.Element -> Either ElementError a
parseElement (Element run) element =
  run
    (NamespaceRegistry.interpretAttributes (Xml.elementAttributes element) NamespaceRegistry.new)
    element
    ElementDestructionState.new
    & fmap fst

renderElementError :: ElementError -> Text
renderElementError =
  TextBuilder.toText . (\(a, b) -> "/" <> TextBuilder.intercalate "/" (reverse a) <> ": " <> b) . simplifyElementError

simplifyElementError :: ElementError -> ([TextBuilder], TextBuilder)
simplifyElementError =
  elementError []
  where
    sortedList renderer =
      mappend "[" . flip mappend "]" . TextBuilder.intercalate ", " . fmap renderer . sort
    name a b =
      case a of
        Just _ -> TextBuilder.text b
        Nothing -> TextBuilder.text b
    elementError collectedPath = \case
      NoneOfChildrenFoundByNameElementError a b ->
        ( collectedPath,
          "None of following child element names found: "
            <> sortedList (uncurry name) a
            <> ". Names available: "
            <> sortedList (uncurry name) b
        )
      ChildByNameElementError a b c ->
        elementError (name a b : collectedPath) c
      ChildAtOffsetElementError a b ->
        nodeError (TextBuilder.decimal a : collectedPath) b
      AttributeByNameElementError a b c ->
        (("@" <> name a b) : collectedPath, maybeContentError c)
      NoneOfAttributesFoundByNameElementError a b ->
        ( collectedPath,
          "Found none of the following attributes: "
            <> sortedList (uncurry name) a
            <> ". The following are available: "
            <> sortedList (uncurry name) b
        )
      NameElementError a ->
        (collectedPath, TextBuilder.text a)
      UserElementError a ->
        (collectedPath, TextBuilder.text a)
    nodeError collectedPath = \case
      UnexpectedNodeTypeNodeError a b ->
        ( collectedPath,
          "Unexpected node type. Got " <> nodeType b <> ", but expected " <> nodeType a
        )
      NotAvailableNodeError ->
        (collectedPath, "No nodes left")
      ElementNodeError a ->
        elementError collectedPath a
      TextNodeError a ->
        (collectedPath, maybeContentError a)
    nodeType = \case
      ElementNodeType -> "element"
      InstructionNodeType -> "instruction"
      ContentNodeType -> "content"
      CommentNodeType -> "comment"
    maybeContentError = maybe "Empty alternative" contentError
    contentError = \case
      UserContentError a ->
        TextBuilder.text a
      ParsingContentError a ->
        TextBuilder.text a
      NamespaceNotFoundContentError a ->
        "Namespace not found: " <> TextBuilder.text a
      UnexpectedValueContentError a ->
        "Unexpected value: " <> TextBuilder.text a
      EnumContentError a b ->
        "Unexpected value: " <> TextBuilder.text b <> ". Expecting one of the following: " <> sortedList TextBuilder.text a

-- |
-- Error in the context of an element.
--
-- It has a tree structure specifying the context of containing operations.
data ElementError
  = AttributeByNameElementError
      (Maybe Text)
      Text
      (Maybe ContentError)
  | NoneOfAttributesFoundByNameElementError
      -- | Not found.
      [(Maybe Text, Text)]
      -- | Out of.
      [(Maybe Text, Text)]
  | NoneOfChildrenFoundByNameElementError
      -- | Not found.
      [(Maybe Text, Text)]
      -- | Out of.
      [(Maybe Text, Text)]
  | ChildByNameElementError
      -- | Namespace.
      (Maybe Text)
      -- | Name.
      Text
      -- | Reason. Not 'NodeError' because only element nodes can be looked up by name.
      ElementError
  | ChildAtOffsetElementError
      -- | Offset.
      Int
      -- | Reason.
      NodeError
  | NameElementError Text
  | -- | Error raised by the user of this library.
    UserElementError Text

data NodeError
  = UnexpectedNodeTypeNodeError
      -- | Expected.
      NodeType
      -- | Actual.
      NodeType
  | NotAvailableNodeError
  | ElementNodeError ElementError
  | TextNodeError (Maybe ContentError)

data ContentError
  = ParsingContentError Text
  | NamespaceNotFoundContentError Text
  | UnexpectedValueContentError Text
  | EnumContentError
      -- | List of expected values.
      [Text]
      -- | Actual value
      Text
  | UserContentError Text

data NodeType
  = ElementNodeType
  | InstructionNodeType
  | ContentNodeType
  | CommentNodeType

-- |
-- Parse in the context of an element node.
newtype Element a
  = Element
      ( NamespaceRegistry.NamespaceRegistry ->
        Xml.Element ->
        ElementDestructionState.ElementDestructionState ->
        Either ElementError (a, ElementDestructionState.ElementDestructionState)
      )
  deriving
    (Functor, Applicative, Monad)
    via ( ReaderT
            (NamespaceRegistry.NamespaceRegistry)
            ( ReaderT
                Xml.Element
                ( StateT
                    ElementDestructionState.ElementDestructionState
                    (Except ElementError)
                )
            )
        )

instance MonadFail Element where
  fail = fromString >>> UserElementError >>> Left >>> const >>> const >>> const >>> Element

-- |
-- Parse namespace and name with the given function.
elementName :: (Maybe Text -> Text -> Either Text a) -> Element a
elementName parse =
  Element $ \nreg (Xml.Element name _ _) state ->
    fmap (,state) $ case NamespaceRegistry.resolveElementName name nreg of
      Nothing -> Left (NameElementError ("Unresolvable name: " <> fromString (show name)))
      Just (ns, name) -> parse ns name & first NameElementError

-- |
-- Fail if the namespace and name don't match the provided.
elementNameIs :: Maybe Text -> Text -> Element ()
elementNameIs ns name =
  elementName $ \actualNs actualName ->
    if actualNs == ns
      then
        if actualName == name
          then Right ()
          else Left ("Unexpected name: \"" <> actualName <> "\". Expecting: \"" <> name <> "\"")
      else Left ("Unexpected namespace: \"" <> (fromString . show) actualNs <> "\". Expecting: \"" <> (fromString . show) ns <> "\"")

-- |
-- Look up elements by name and parse them.
childrenByName :: ByName Element a -> Element a
childrenByName (ByName runByName) =
  Element $ \nreg element@(Xml.Element _ attributes _) state ->
    case ElementDestructionState.resolveChildNames (ElementDestructionState.ElementDestructionContext nreg element) state of
      (nameMap, state) ->
        case runByName nameMap (\element (Element run) -> fmap fst (run deeperNreg element ElementDestructionState.new)) of
          OkByNameResult _ res -> Right (res, state)
          NotFoundByNameResult unfoundNames ->
            let availNames = nub $ NameMap.extractNames nameMap
             in Left (NoneOfChildrenFoundByNameElementError unfoundNames availNames)
          FailedDeeperByNameResult ns name err ->
            Left (ChildByNameElementError ns name err)
        where
          deeperNreg = NamespaceRegistry.interpretAttributes attributes nreg

-- |
-- Look up the last attribute by name and parse it.
attributesByName :: ByName Content a -> Element a
attributesByName (ByName runByName) =
  Element $ \nreg element state ->
    case ElementDestructionState.resolveAttributeNames (ElementDestructionState.ElementDestructionContext nreg element) state of
      (nameMap, state) -> case runByName nameMap (\content (Content parseContent) -> parseContent (\ns -> NamespaceRegistry.lookup ns nreg) content) of
        OkByNameResult _ res -> Right (res, state)
        NotFoundByNameResult unfoundNames ->
          let availNames = nub $ NameMap.extractNames nameMap
           in Left (NoneOfAttributesFoundByNameElementError unfoundNames availNames)
        FailedDeeperByNameResult ns name err ->
          Left (AttributeByNameElementError ns name err)

-- |
-- Children sequence by order.
children :: Nodes a -> Element a
children (Nodes runNodes) =
  Element $ \nreg (Xml.Element _ _ nodes) state ->
    runNodes (NodeConsumerState.new nodes nreg)
      & fmap fst
      & fmap (,state)

-- |
-- Expose the element's AST.
astElement :: Element Xml.Element
astElement =
  Element $ \_ element state -> Right (element, state)

-- |
-- Parser in the context of a sequence of nodes.
newtype Nodes a
  = Nodes (NodeConsumerState.NodeConsumerState -> Either ElementError (a, NodeConsumerState.NodeConsumerState))
  deriving
    (Functor, Applicative, Monad)
    via (StateT NodeConsumerState.NodeConsumerState (Either ElementError))

-- |
-- Consume the next node expecting it to be element and parse its contents.
elementNode :: Element a -> Nodes a
elementNode (Element runElement) =
  Nodes $ \x ->
    case NodeConsumerState.fetchNode x of
      Just (node, x) -> case node of
        Xml.NodeElement element ->
          bimap
            (ChildAtOffsetElementError (NodeConsumerState.getOffset x) . ElementNodeError)
            (,NodeConsumerState.bumpOffset x)
            (fmap fst (runElement (NodeConsumerState.getNamespaceRegistry x) element ElementDestructionState.new))
        Xml.NodeInstruction _ -> failWithUnexpectedNodeType InstructionNodeType
        Xml.NodeContent _ -> failWithUnexpectedNodeType ContentNodeType
        Xml.NodeComment _ -> failWithUnexpectedNodeType CommentNodeType
        where
          failWithUnexpectedNodeType actualType =
            Left
              ( ChildAtOffsetElementError
                  (NodeConsumerState.getOffset x)
                  (UnexpectedNodeTypeNodeError ElementNodeType actualType)
              )
      _ -> Left (ChildAtOffsetElementError (NodeConsumerState.getOffset x) NotAvailableNodeError)

-- |
-- Consume the next node expecting it to be textual and parse its contents.
contentNode :: Content content -> Nodes content
contentNode (Content parseContent) =
  Nodes $ \x ->
    case NodeConsumerState.fetchNode x of
      Just (node, x) -> case node of
        Xml.NodeContent content ->
          case parseContent (\ns -> NodeConsumerState.lookupNamespace ns x) content of
            Right parsedContent ->
              Right (parsedContent, NodeConsumerState.bumpOffset x)
            Left contentError ->
              Left
                ( ChildAtOffsetElementError
                    (NodeConsumerState.getOffset x)
                    (TextNodeError contentError)
                )
        Xml.NodeElement _ -> failWithUnexpectedNodeType ElementNodeType
        Xml.NodeInstruction _ -> failWithUnexpectedNodeType InstructionNodeType
        Xml.NodeComment _ -> failWithUnexpectedNodeType CommentNodeType
        where
          failWithUnexpectedNodeType actualType =
            Left
              ( ChildAtOffsetElementError
                  (NodeConsumerState.getOffset x)
                  (UnexpectedNodeTypeNodeError ContentNodeType actualType)
              )
      _ ->
        case NodeConsumerState.getOffset x of
          0 ->
            case parseContent (\ns -> NodeConsumerState.lookupNamespace ns x) mempty of
              Right parsedContent ->
                Right (parsedContent, NodeConsumerState.bumpOffset x)
              Left contentError ->
                Left (ChildAtOffsetElementError 0 (TextNodeError contentError))
          offset ->
            Left (ChildAtOffsetElementError offset NotAvailableNodeError)

-- * Content

-- |
-- Parser in the context of decoded textual content,
-- which can be the value of an attribute or a textual node.
newtype Content content
  = -- | Parser in the context of an xml namespace URI by alias lookup function.
    Content ((Text -> Maybe Text) -> Text -> Either (Maybe ContentError) content)
  deriving
    (Functor, Applicative, Monad, Alternative, MonadPlus)
    via (ReaderT (Text -> Maybe Text) (ExceptT (Last ContentError) ((->) Text)))

instance MonadFail Content where
  fail = fromString >>> UserContentError >>> Just >>> Left >>> const >>> const >>> Content

-- |
-- Return the content as it is.
textContent :: Content Text
textContent =
  Content (const pure)

-- |
-- Map the content to a type if it's valid.
narrowedContent :: (Text -> Maybe a) -> Content a
narrowedContent mapper =
  Content (const (\x -> maybe (Left (Just (UnexpectedValueContentError x))) Right (mapper x)))

-- |
-- Parse the content with a possibly failing function.
refinedContent :: (Text -> Either Text a) -> Content a
refinedContent refine =
  Content (const (first (Just . ParsingContentError) . refine))

-- |
-- Map the content using a dictionary.
enumContent :: [(Text, a)] -> Content a
enumContent mappingList =
  let !expectedKeysList =
        fmap fst mappingList
      mappingListLength =
        length mappingList
      !narrow =
        if mappingListLength > 512
          then
            let !hashMap = HashMap.fromList mappingList
             in flip HashMap.lookup hashMap
          else flip List.lookup mappingList
      extract a =
        case narrow a of
          Just b -> Right b
          _ -> Left (Just (EnumContentError expectedKeysList a))
   in Content (const extract)

-- |
-- Parse the content using the \"attoparsec\" parser.
attoparsedContent :: Attoparsec.Parser a -> Content a
attoparsedContent parser =
  Content (const (first (Just . ParsingContentError . fromString) . Attoparsec.parseOnly parser))

-- |
-- Parse the content as XML Schema QName,
-- automatically resolving the namespace as URI and failing,
-- if none is associated.
--
-- Produces a URI associated with the namespace and name.
-- If the content does not contain colon, produces an unnamespaced name.
--
-- Refs:
--
-- - https://www.w3.org/2001/tag/doc/qnameids.html#sec-qnames-xml
-- - https://en.wikipedia.org/wiki/QName
qNameContent :: Content (Maybe Text, Text)
qNameContent =
  Content $ \lookup content -> case Attoparsec.parseStripped Attoparsec.qName content of
    Right (ns, name) -> case ns of
      Just ns -> case lookup ns of
        Just uri -> Right (Just uri, name)
        Nothing -> Left (Just (NamespaceNotFoundContentError ns))
      Nothing -> Right (Nothing, name)
    Left err -> Left (Just (ParsingContentError err))

-- * ByName

data ByNameResult deeperError content a
  = NotFoundByNameResult [(Maybe Text, Text)]
  | FailedDeeperByNameResult (Maybe Text) Text deeperError
  | OkByNameResult (NameMap.NameMap content) a
  deriving (Functor)

-- |
-- Composable extension to a parser, which looks up its input by name.
--
-- Useful for searching elements and attributes by name.
--
-- Alternative and MonadPlus alternate only on lookup errors.
-- When lookup is successful, but the deeper parser fails,
-- the error propagates.
--
-- Monad and Applicative sequentially fetch contents by matching names.
newtype ByName parser a
  = ByName
      ( forall content deeperError.
        NameMap.NameMap content ->
        (content -> forall x. parser x -> Either deeperError x) ->
        ByNameResult deeperError content a
      )

instance Functor (ByName parser) where
  fmap fn (ByName run) =
    ByName $ \map parse -> fmap fn (run map parse)

instance Applicative (ByName parser) where
  pure x =
    ByName $ \map _ -> OkByNameResult map x
  ByName runL <*> ByName runR =
    ByName $ \map parse -> case runL map parse of
      OkByNameResult map lRes -> runR map parse & fmap lRes
      NotFoundByNameResult unfoundNames -> NotFoundByNameResult unfoundNames
      FailedDeeperByNameResult ns name err -> FailedDeeperByNameResult ns name err

instance Monad (ByName parser) where
  return = pure
  ByName runL >>= k =
    ByName $ \map parse -> case runL map parse of
      OkByNameResult map lRes -> case k lRes of ByName runR -> runR map parse
      NotFoundByNameResult unfoundNames -> NotFoundByNameResult unfoundNames
      FailedDeeperByNameResult ns name err -> FailedDeeperByNameResult ns name err

instance Alternative (ByName parser) where
  empty =
    ByName $ \_ _ -> NotFoundByNameResult []
  ByName runL <|> ByName runR =
    ByName $ \map parse -> case runL map parse of
      OkByNameResult map lRes -> OkByNameResult map lRes
      NotFoundByNameResult unfoundNamesL -> case runR map parse of
        NotFoundByNameResult unfoundNamesR -> NotFoundByNameResult (unfoundNamesL <> unfoundNamesR)
        resR -> resR
      FailedDeeperByNameResult ns name err -> FailedDeeperByNameResult ns name err

instance MonadPlus (ByName parser) where
  mzero = empty
  mplus = (<|>)

-- |
-- Execute a parser on the result of looking up a content by namespace and name.
byName :: Maybe Text -> Text -> parser a -> ByName parser a
byName ns name parser =
  ByName $ \map parse ->
    case NameMap.fetch ns name map of
      Just (content, map) -> case parse content parser of
        Right a -> OkByNameResult map a
        Left err -> FailedDeeperByNameResult ns name err
      Nothing -> NotFoundByNameResult [(ns, name)]
