{-# LANGUAGE OverloadedStrings #-}

-- | Parser for the human-readable GRIN syntax emitted by
-- "Aihc.Grin.Pretty".
module Aihc.Grin.Parser
  ( GrinParseError,
    parseProgram,
    parseExpr,
    renderParseError,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types
  ( Levity (..),
    RuntimeRep (..),
    Unique (..),
    VecCount (..),
    VecElem (..),
    liftedRuntimeRep,
  )
import Control.Applicative (optional, (<|>))
import Control.Monad (guard, void, when)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isSpace, ord)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

type GrinParseError = ParseErrorBundle Text Void

parseProgram :: Text -> Either GrinParseError GrinProgram
parseProgram = MP.parse programParser "<grin>"

parseExpr :: Text -> Either GrinParseError GrinExpr
parseExpr = MP.parse (exprAfterIndent 0 <* MP.eof) "<grin-expression>"

renderParseError :: GrinParseError -> String
renderParseError = MP.errorBundlePretty

data TopDeclaration
  = TopConstructor (Text, [[RuntimeRep]])
  | TopPrimitive (GrinVar, Int)
  | TopForeign GrinForeignCall
  | TopExternalGlobal Text
  | TopExternalFunction GrinCodeInfo
  | TopWhnfGlobal (GrinVar, GrinNode)
  | TopCaf (GrinVar, GrinNode)
  | TopFunction GrinFunction

programParser :: Parser GrinProgram
programParser = do
  blankLines
  declarations <- MP.many (topDeclaration <* blankLines)
  MP.eof
  pure (foldl' addDeclaration emptyProgram declarations)

emptyProgram :: GrinProgram
emptyProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions = []
    }

addDeclaration :: GrinProgram -> TopDeclaration -> GrinProgram
addDeclaration program declaration =
  case declaration of
    TopConstructor value -> program {grinConstructors = grinConstructors program <> [value]}
    TopPrimitive value -> program {grinPrimitives = grinPrimitives program <> [value]}
    TopForeign value -> program {grinForeignCalls = grinForeignCalls program <> [value]}
    TopExternalGlobal value -> program {grinExternalGlobals = grinExternalGlobals program <> [value]}
    TopExternalFunction value -> program {grinExternalFunctions = grinExternalFunctions program <> [value]}
    TopWhnfGlobal value -> program {grinWhnfGlobals = grinWhnfGlobals program <> [value]}
    TopCaf value -> program {grinCafs = grinCafs program <> [value]}
    TopFunction value -> program {grinFunctions = grinFunctions program <> [value]}

topDeclaration :: Parser TopDeclaration
topDeclaration = do
  exactIndent 0
  MP.choice
    [ MP.try constructorDeclaration,
      MP.try primitiveDeclaration,
      MP.try foreignDeclaration,
      MP.try externalGlobalDeclaration,
      MP.try externalFunctionDeclaration,
      MP.try (TopWhnfGlobal <$> staticDeclaration "global"),
      MP.try (TopCaf <$> staticDeclaration "caf"),
      TopFunction <$> functionDeclaration
    ]

constructorDeclaration :: Parser TopDeclaration
constructorDeclaration = do
  keyword "constructor"
  horizontal1
  constructorName <- name
  _ <- MPC.char '/'
  arity <- natural
  horizontal1
  fieldLayouts <- constructorLayouts
  lineEnd
  when (arity /= length fieldLayouts) $ fail "constructor arity does not match its layout"
  pure (TopConstructor (constructorName, fieldLayouts))

primitiveDeclaration :: Parser TopDeclaration
primitiveDeclaration = do
  keyword "primitive"
  horizontal1
  variable <- bareVar
  _ <- MPC.char '/'
  arity <- signedInt
  lineEnd
  pure (TopPrimitive (variable, arity))

foreignDeclaration :: Parser TopDeclaration
foreignDeclaration = do
  keyword "foreign"
  horizontal1
  foreignCall <- foreignCallDefinition
  lineEnd
  pure (TopForeign foreignCall)

externalGlobalDeclaration :: Parser TopDeclaration
externalGlobalDeclaration = do
  keyword "external"
  horizontal1
  keyword "global"
  horizontal1
  TopExternalGlobal <$> name <* lineEnd

externalFunctionDeclaration :: Parser TopDeclaration
externalFunctionDeclaration = do
  keyword "external"
  horizontal1
  sourceName <- name
  _ <- MPC.char '/'
  arity <- natural
  horizontal1
  parameterLayouts <- layouts
  horizontal1
  _ <- MPC.string "->"
  horizontal1
  resultRep <- runtimeRep
  horizontal1
  _ <- MPC.char '='
  horizontal1
  functionName <- FunctionName <$> name
  lineEnd
  when (arity /= length parameterLayouts) $ fail "external function arity does not match its layouts"
  pure
    ( TopExternalFunction
        GrinCodeInfo
          { grinCodeSourceName = sourceName,
            grinCodeFunctionName = functionName,
            grinCodeParameterLayouts = parameterLayouts,
            grinCodeResultRep = resultRep
          }
    )

staticDeclaration :: Text -> Parser (GrinVar, GrinNode)
staticDeclaration declarationName = do
  keyword declarationName
  horizontal1
  variable <- bareVar
  horizontal1
  _ <- MPC.char '='
  horizontal1
  node <- grinNode
  lineEnd
  pure (variable, node)

functionDeclaration :: Parser GrinFunction
functionDeclaration = do
  functionName <- FunctionName <$> name
  parameters <- MP.many (MP.try (horizontal1 *> varAtom))
  horizontal1
  _ <- MPC.string "->"
  horizontal1
  resultRep <- runtimeRep
  linkName <- optional (MP.try (horizontal1 *> keyword "link" *> horizontal1 *> name))
  horizontal1
  _ <- MPC.char '='
  lineEnd
  body <- nestedExpr 0
  pure
    GrinFunction
      { grinFunctionName = functionName,
        grinFunctionLinkName = linkName,
        grinFunctionParameters = parameters,
        grinFunctionResultRep = resultRep,
        grinFunctionBody = body
      }

exprAt :: Int -> Parser GrinExpr
exprAt indentation = exactIndent indentation *> exprAfterIndent indentation

nestedExpr :: Int -> Parser GrinExpr
nestedExpr parentIndentation = do
  indentation <- indentationLevel
  when (indentation <= parentIndentation) $ fail "expected an indented GRIN expression"
  exprAfterIndent indentation

exprAfterIndent :: Int -> Parser GrinExpr
exprAfterIndent indentation =
  MP.choice
    [ bindExpr indentation,
      caseExpr indentation,
      MP.try (storeRecExpr indentation "store-rec-unchecked" GrinStoreRecUnchecked),
      MP.try (storeRecExpr indentation "store-rec" GrinStoreRec),
      atomicExpr
    ]

bindExpr :: Int -> Parser GrinExpr
bindExpr indentation = do
  binders <- MP.try (binderList <* horizontal1 <* MPC.string "<-")
  valueExpression <-
    MP.try (lineEnd *> nestedExpr indentation)
      <|> (horizontal1 *> atomicExpr)
  blankLines
  body <- exprAt indentation
  pure (GrinBind binders valueExpression body)

binderList :: Parser [GrinVar]
binderList =
  [] <$ MPC.string "()"
    <|> binderVar `MP.sepBy1` commaSeparator

binderVar :: Parser GrinVar
binderVar = MP.try varAtom <|> bareVar

caseExpr :: Int -> Parser GrinExpr
caseExpr indentation = do
  keyword "case"
  horizontal1
  scrutinee <- grinValue
  horizontal1
  keyword "as"
  horizontal1
  binder <- bareVar
  horizontal1
  keyword "of"
  lineEnd
  alternatives <- grinAlternatives indentation binder
  pure (GrinCase scrutinee binder alternatives)

grinAlternatives :: Int -> GrinVar -> Parser [GrinAlt]
grinAlternatives parentIndentation binder = do
  next <- optional (MP.try (MP.lookAhead deeperIndentation))
  case next of
    Nothing -> pure []
    Just indentation -> alternativesAt indentation
  where
    deeperIndentation = do
      indentation <- indentationLevel
      guard (indentation > parentIndentation)
      pure indentation
    alternativesAt indentation = do
      blankLines
      hasAlternative <- optional (MP.try (MP.lookAhead (exactIndent indentation)))
      case hasAlternative of
        Nothing -> pure []
        Just () -> (:) <$> alternativeAt indentation binder <*> alternativesAt indentation

alternativeAt :: Int -> GrinVar -> Parser GrinAlt
alternativeAt indentation binder = do
  exactIndent indentation
  constructor <- altConstructor binder
  binders <- MP.many (MP.try (horizontal1 *> varAtom))
  horizontal1
  _ <- MPC.string "->"
  lineEnd
  rhs <- nestedExpr indentation
  pure
    GrinAlt
      { grinAltCon = constructor,
        grinAltBinders = binders,
        grinAltRhs = rhs
      }

altConstructor :: GrinVar -> Parser GrinAltCon
altConstructor binder =
  MP.try (GrinDataAlt <$> (keyword "data" *> horizontal1 *> name))
    <|> GrinDefaultAlt <$ MPC.char '_'
    <|> MP.try (GrinLitAlt <$> grinLiteral)
    <|> MP.try (GrinLitAlt . GrinLitInt (grinVarRuntimeRep binder) <$> signedInteger)
    <|> GrinDataAlt <$> name

storeRecExpr :: Int -> Text -> ([(GrinVar, GrinNode)] -> GrinExpr -> GrinExpr) -> Parser GrinExpr
storeRecExpr indentation expressionName constructor = do
  keyword expressionName
  lineEnd
  blankLines
  next <- optional (MP.try (MP.lookAhead deeperIndentation))
  bindings <-
    case next of
      Nothing -> pure []
      Just bindingIndentation -> bindingsAt bindingIndentation
  body <- exprAt indentation
  pure (constructor bindings body)
  where
    deeperIndentation = do
      bindingIndentation <- indentationLevel
      guard (bindingIndentation > indentation)
      pure bindingIndentation
    bindingsAt bindingIndentation = do
      hasBinding <- optional (MP.try (MP.lookAhead (exactIndent bindingIndentation)))
      case hasBinding of
        Nothing -> pure []
        Just () -> (:) <$> storeBindingAt bindingIndentation <*> bindingsAt bindingIndentation

storeBindingAt :: Int -> Parser (GrinVar, GrinNode)
storeBindingAt indentation = do
  exactIndent indentation
  variable <- binderVar
  horizontal1
  _ <- MPC.char '='
  horizontal1
  node <- grinNode
  lineEnd
  pure (variable, node)

atomicExpr :: Parser GrinExpr
atomicExpr =
  MP.choice
    [ unaryValuesExpr "constant" GrinConstant,
      MP.try (unaryNodeExpr "store-unchecked" GrinStoreUnchecked),
      unaryNodeExpr "store" GrinStore,
      ensureHeapExpr,
      runtimeRepValueExpr "fetch" GrinFetch,
      twoValuesExpr "update-blackhole" GrinUpdateBlackhole,
      twoValuesExpr "update" GrinUpdate,
      runtimeRepValueExpr "eval" GrinEval,
      cpsEvalExpr,
      namedCallExpr "call" GrinCall,
      primitiveCallExpr,
      cpsPrimitiveCallExpr,
      applyExpr,
      cpsApplyExpr,
      continueExpr,
      unaryValuesExpr "halt" GrinHalt,
      unaryValueExpr "throw" GrinThrow,
      catchExpr,
      foreignCallExpr
    ]

unaryValuesExpr :: Text -> ([GrinValue] -> GrinExpr) -> Parser GrinExpr
unaryValuesExpr expressionName constructor = do
  keyword expressionName
  values <- grinValues
  lineEnd
  pure (constructor values)

unaryNodeExpr :: Text -> (GrinNode -> GrinExpr) -> Parser GrinExpr
unaryNodeExpr expressionName constructor = do
  keyword expressionName
  horizontal1
  node <- grinNode
  lineEnd
  pure (constructor node)

unaryValueExpr :: Text -> (GrinValue -> GrinExpr) -> Parser GrinExpr
unaryValueExpr expressionName constructor = do
  keyword expressionName
  horizontal1
  value <- grinValue
  lineEnd
  pure (constructor value)

ensureHeapExpr :: Parser GrinExpr
ensureHeapExpr = do
  keyword "ensure-heap"
  horizontal1
  requiredWords <- signedInt
  roots <- grinValues
  lineEnd
  pure (GrinEnsureHeap requiredWords roots)

runtimeRepValueExpr :: Text -> (RuntimeRep -> GrinValue -> GrinExpr) -> Parser GrinExpr
runtimeRepValueExpr expressionName constructor = do
  keyword expressionName
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  value <- grinValue
  lineEnd
  pure (constructor representation value)

twoValuesExpr :: Text -> (GrinValue -> GrinValue -> GrinExpr) -> Parser GrinExpr
twoValuesExpr expressionName constructor = do
  keyword expressionName
  horizontal1
  first <- grinValue
  horizontal1
  second <- grinValue
  lineEnd
  pure (constructor first second)

cpsEvalExpr :: Parser GrinExpr
cpsEvalExpr = do
  keyword "cps-eval"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  value <- grinValue
  horizontal1
  continuation <- grinValue
  horizontal1
  updateContinuation <- grinValue
  lineEnd
  pure (GrinCpsEval representation value continuation updateContinuation)

namedCallExpr :: Text -> (RuntimeRep -> FunctionName -> [GrinValue] -> GrinExpr) -> Parser GrinExpr
namedCallExpr expressionName constructor = do
  keyword expressionName
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  functionName <- FunctionName <$> name
  arguments <- grinValues
  lineEnd
  pure (constructor representation functionName arguments)

primitiveCallExpr :: Parser GrinExpr
primitiveCallExpr = do
  keyword "primitive-call"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  primitiveName <- name
  arguments <- grinValues
  lineEnd
  pure (GrinPrimitiveCall representation primitiveName arguments)

cpsPrimitiveCallExpr :: Parser GrinExpr
cpsPrimitiveCallExpr = do
  keyword "cps-primitive-call"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  primitiveName <- name
  arguments <- MP.many (MP.try (horizontal1 *> grinValue))
  horizontal1
  _ <- MPC.string "->"
  horizontal1
  continuation <- grinValue
  lineEnd
  pure (GrinCpsPrimitiveCall representation primitiveName arguments continuation)

applyExpr :: Parser GrinExpr
applyExpr = do
  keyword "apply"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  function <- grinValue
  horizontal1
  arguments <- grinArgument
  lineEnd
  pure (GrinApply representation function arguments)

cpsApplyExpr :: Parser GrinExpr
cpsApplyExpr = do
  keyword "cps-apply"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  function <- grinValue
  horizontal1
  arguments <- grinArgument
  horizontal1
  _ <- MPC.string "->"
  horizontal1
  continuation <- grinValue
  lineEnd
  pure (GrinCpsApply representation function arguments continuation)

continueExpr :: Parser GrinExpr
continueExpr = do
  keyword "continue"
  horizontal1
  continuation <- grinValue
  horizontal1
  values <- grinArgument
  lineEnd
  pure (GrinContinue continuation values)

catchExpr :: Parser GrinExpr
catchExpr = do
  keyword "catch"
  horizontal1
  representation <- runtimeRepArgument
  horizontal1
  action <- grinValue
  horizontal1
  handler <- grinValue
  state <- grinValues
  lineEnd
  pure (GrinCatch representation action handler state)

foreignCallExpr :: Parser GrinExpr
foreignCallExpr = do
  keyword "foreign-call"
  horizontal1
  foreignCall <- foreignCallDefinition
  horizontal1
  keyword "with"
  arguments <- grinValues
  lineEnd
  pure (GrinForeignCallExpr foreignCall arguments)

grinValues :: Parser [GrinValue]
grinValues = MP.many (MP.try (horizontal1 *> grinValue))

grinArgument :: Parser [GrinValue]
grinArgument =
  MP.try ((: []) <$> grinValue)
    <|> betweenHorizontal '(' ')' (MP.many (grinValue <* horizontal))

grinNode :: Parser GrinNode
grinNode = betweenHorizontal '(' ')' $ do
  tag <- nodeTag
  GrinNode tag <$> grinValues

nodeTag :: Parser GrinNodeTag
nodeTag =
  MP.choice
    [ do
        _ <- MPC.char 'C'
        constructorName <- name
        remaining <- optional (MPC.char '/' *> signedInt)
        pure (GrinConstructor constructorName (fromMaybe 0 remaining)),
      do
        _ <- MPC.char 'P'
        functionName <- FunctionName <$> name
        _ <- MPC.char '/'
        argumentLayouts <-
          MP.try layouts <|> do
            arity <- natural
            exactLayouts <- optional layouts
            let parsedLayouts = fromMaybe (replicate arity [liftedRuntimeRep]) exactLayouts
            when (arity /= length parsedLayouts) $ fail "closure arity does not match its layouts"
            pure parsedLayouts
        pure (GrinClosure functionName argumentLayouts),
      GrinThunk . FunctionName <$> (MPC.char 'F' *> name)
    ]

grinValue :: Parser GrinValue
grinValue =
  MP.try (GrinVarValue <$> varAtom)
    <|> GrinLitValue <$> grinLiteral

grinLiteral :: Parser GrinLiteral
grinLiteral =
  MP.choice
    [ MP.try typedIntegerLiteral,
      MP.try typedCharLiteral,
      MP.try addressLiteral,
      GrinLitString <$> stringText
    ]

typedIntegerLiteral :: Parser GrinLiteral
typedIntegerLiteral = betweenHorizontal '(' ')' $ do
  value <- signedInteger
  horizontal1
  _ <- MPC.string "::"
  horizontal1
  GrinLitInt <$> runtimeRep <*> pure value

typedCharLiteral :: Parser GrinLiteral
typedCharLiteral = betweenHorizontal '(' ')' $ do
  value <- haskellChar
  horizontal1
  _ <- MPC.string "::"
  horizontal1
  GrinLitChar <$> runtimeRep <*> pure value

addressLiteral :: Parser GrinLiteral
addressLiteral = do
  value <- stringText
  _ <- MPC.char '#'
  pure (GrinLitAddr (BS.pack (map (fromIntegral . ord) (T.unpack value))))

varAtom :: Parser GrinVar
varAtom = betweenHorizontal '(' ')' bareVar

bareVar :: Parser GrinVar
bareVar = do
  variableName <- name
  _ <- MPC.char '%'
  uniqueValue <- signedInt
  horizontal1
  _ <- MPC.string "::"
  horizontal1
  GrinVar variableName uniqueValue <$> runtimeRep

foreignCallDefinition :: Parser GrinForeignCall
foreignCallDefinition = do
  callName <- name
  horizontal1
  _ <- MPC.char '='
  horizontal1
  symbolName <- stringText
  horizontal1
  _ <- MPC.string "::"
  horizontal1
  signature <- foreignSignature
  pure
    GrinForeignCall
      { grinForeignCallName = callName,
        grinForeignCallSymbol = symbolName,
        grinForeignCallSignature = signature
      }

foreignSignature :: Parser GrinForeignSignature
foreignSignature = do
  argumentTypes <- betweenHorizontal '(' ')' (foreignType `MP.sepBy` commaSeparator)
  horizontal1
  _ <- MPC.string "->"
  horizontal1
  resultType <- foreignType
  horizontal1
  _ <- MPC.char '!'
  horizontal1
  effect <-
    GrinForeignPure <$ keyword "pure"
      <|> GrinForeignRealWorld <$ keyword "real-world"
  pure
    GrinForeignSignature
      { grinForeignArgumentTypes = argumentTypes,
        grinForeignResultType = resultType,
        grinForeignEffect = effect
      }

foreignType :: Parser GrinForeignType
foreignType =
  MP.choice
    [ GrinForeignInt32 <$ keyword "int32",
      GrinForeignWord64 <$ keyword "word64",
      GrinForeignAddr <$ keyword "addr"
    ]

runtimeRepArgument :: Parser RuntimeRep
runtimeRepArgument = MPC.char '@' *> runtimeRep

runtimeRep :: Parser RuntimeRep
runtimeRep =
  MP.try (betweenHorizontal '(' ')' runtimeRep)
    <|> MP.choice
      [ VecRep <$ keyword "VecRep" <* horizontal1 <*> vecCount <* horizontal1 <*> vecElem,
        TupleRep <$ keyword "TupleRep" <* horizontal1 <*> runtimeRepList,
        SumRep <$ keyword "SumRep" <* horizontal1 <*> runtimeRepList,
        BoxedRep <$ keyword "BoxedRep" <* horizontal1 <*> levity,
        RuntimeRepVar <$ keyword "RuntimeRepVar" <* horizontal1 <*> unique,
        RuntimeRepMeta <$ keyword "RuntimeRepMeta" <* horizontal1 <*> unique,
        Int8Rep <$ keyword "Int8Rep",
        Int16Rep <$ keyword "Int16Rep",
        Int32Rep <$ keyword "Int32Rep",
        Int64Rep <$ keyword "Int64Rep",
        IntRep <$ keyword "IntRep",
        Word8Rep <$ keyword "Word8Rep",
        Word16Rep <$ keyword "Word16Rep",
        Word32Rep <$ keyword "Word32Rep",
        Word64Rep <$ keyword "Word64Rep",
        WordRep <$ keyword "WordRep",
        AddrRep <$ keyword "AddrRep",
        FloatRep <$ keyword "FloatRep",
        DoubleRep <$ keyword "DoubleRep"
      ]

runtimeRepList :: Parser [RuntimeRep]
runtimeRepList = betweenHorizontal '[' ']' (runtimeRep `MP.sepBy` commaSeparator)

constructorLayouts :: Parser [[RuntimeRep]]
constructorLayouts =
  betweenHorizontal '[' ']' (constructorLayout `MP.sepBy` commaSeparator)
  where
    constructorLayout = runtimeRepList <|> ((: []) <$> runtimeRep)

layouts :: Parser [[RuntimeRep]]
layouts = betweenHorizontal '[' ']' (runtimeRepList `MP.sepBy` commaSeparator)

levity :: Parser Levity
levity = Lifted <$ keyword "Lifted" <|> Unlifted <$ keyword "Unlifted"

vecCount :: Parser VecCount
vecCount =
  MP.choice
    [ Vec2 <$ keyword "Vec2",
      Vec4 <$ keyword "Vec4",
      Vec8 <$ keyword "Vec8",
      Vec16 <$ keyword "Vec16",
      Vec32 <$ keyword "Vec32",
      Vec64 <$ keyword "Vec64"
    ]

vecElem :: Parser VecElem
vecElem =
  MP.choice
    [ Int8ElemRep <$ keyword "Int8ElemRep",
      Int16ElemRep <$ keyword "Int16ElemRep",
      Int32ElemRep <$ keyword "Int32ElemRep",
      Int64ElemRep <$ keyword "Int64ElemRep",
      Word8ElemRep <$ keyword "Word8ElemRep",
      Word16ElemRep <$ keyword "Word16ElemRep",
      Word32ElemRep <$ keyword "Word32ElemRep",
      Word64ElemRep <$ keyword "Word64ElemRep",
      FloatElemRep <$ keyword "FloatElemRep",
      DoubleElemRep <$ keyword "DoubleElemRep"
    ]

unique :: Parser Unique
unique = betweenHorizontal '(' ')' (Unique <$ keyword "Unique" <* horizontal1 <*> signedIntAtom)

signedIntAtom :: Parser Int
signedIntAtom = MP.try (betweenHorizontal '(' ')' signedInt) <|> signedInt

name :: Parser Text
name = stringText <|> MP.takeWhile1P (Just "name") isBareNameCharacter
  where
    isBareNameCharacter character =
      not (isSpace character)
        && character `notElem` ['"', '(', ')', '[', ']', ',', '=', '/', '%']

stringText :: Parser Text
stringText = T.pack <$> (MPC.char '"' *> MP.manyTill L.charLiteral (MPC.char '"'))

haskellChar :: Parser Char
haskellChar = MPC.char '\'' *> L.charLiteral <* MPC.char '\''

signedInt :: Parser Int
signedInt = L.signed (pure ()) L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed (pure ()) L.decimal

natural :: Parser Int
natural = L.decimal

keyword :: Text -> Parser ()
keyword text = void (MPC.string text <* MP.notFollowedBy (MP.satisfy isKeywordContinuation))
  where
    isKeywordContinuation character = isAlphaNum character || character == '_'

betweenHorizontal :: Char -> Char -> Parser value -> Parser value
betweenHorizontal opening closing parser = do
  _ <- MPC.char opening
  horizontal
  value <- parser
  horizontal
  _ <- MPC.char closing
  pure value

indentationLevel :: Parser Int
indentationLevel = T.length <$> MP.takeWhileP (Just "indentation") (== ' ')

exactIndent :: Int -> Parser ()
exactIndent expected = do
  actual <- indentationLevel
  when (actual /= expected) $ fail ("expected " <> show expected <> " spaces of indentation")

horizontal :: Parser ()
horizontal = void (MP.takeWhileP (Just "horizontal whitespace") (== ' '))

horizontal1 :: Parser ()
horizontal1 = void (MP.takeWhile1P (Just "horizontal whitespace") (== ' '))

commaSeparator :: Parser Char
commaSeparator = MP.try (horizontal *> MPC.char ',' <* horizontal)

blankLines :: Parser ()
blankLines = void (MP.many (MP.try (horizontal *> MPC.eol)))

lineEnd :: Parser ()
lineEnd = horizontal *> (void MPC.eol <|> MP.eof)
