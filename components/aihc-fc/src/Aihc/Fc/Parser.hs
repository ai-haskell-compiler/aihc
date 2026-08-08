{-# LANGUAGE OverloadedStrings #-}

-- | Parser for the canonical System FC syntax emitted by
-- "Aihc.Fc.Pretty".
module Aihc.Fc.Parser
  ( FcParseError,
    parseProgram,
    parseExpr,
    parseType,
    renderParseError,
  )
where

import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
  ( Kind (..),
    Levity (..),
    Pred (..),
    RuntimeRep (..),
    TcType (..),
    TyCon,
    TyVarId (..),
    Unique (..),
    VecCount (..),
    VecElem (..),
    mkTyCon,
    setTyConKind,
    setTyVarKind,
  )
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (ParseErrorBundle, Parsec)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read (readMaybe)

type Parser = Parsec Void Text

type FcParseError = ParseErrorBundle Text Void

parseProgram :: Text -> Either FcParseError FcProgram
parseProgram = MP.parse (FcProgram <$> (space *> MP.many topBind <* MP.eof)) "<system-fc>"

parseExpr :: Text -> Either FcParseError FcExpr
parseExpr = MP.parse (space *> expression <* MP.eof) "<system-fc-expression>"

parseType :: Text -> Either FcParseError TcType
parseType = MP.parse (space *> tcType <* MP.eof) "<system-fc-type>"

renderParseError :: FcParseError -> String
renderParseError = MP.errorBundlePretty

topBind :: Parser FcTopBind
topBind =
  MP.choice
    [ makeData <$> form "data" ((,,) <$> text <* comma <*> list tyVar <* comma <*> list dataConstructor),
      FcAxiom <$> form "axiom" axiomDecl,
      FcNewtype <$> form "newtype" newtypeDecl,
      uncurry FcPrimitive <$> form "primitive" ((,) <$> var <* comma <*> int),
      FcForeignImport <$> form "foreign-import" foreignCall,
      FcTopBind <$> form "top-bind" bind
    ]
  where
    makeData (name, tyVars, constructors) = FcData name tyVars constructors

dataConstructor :: Parser (Text, [TcType])
dataConstructor = form "constructor" ((,) <$> text <* comma <*> list tcType)

axiomDecl :: Parser FcAxiomDecl
axiomDecl =
  form "axiom-decl" $
    FcAxiomDecl
      <$> text
      <* comma
      <*> list tyVar
      <* comma
      <*> axiomRole
      <* comma
      <*> tcType
      <* comma
      <*> tcType

axiomRole :: Parser FcAxiomRole
axiomRole = FcNominal <$ keyword "nominal" <|> FcRepresentational <$ keyword "representational"

newtypeDecl :: Parser FcNewtypeDecl
newtypeDecl =
  form "newtype-decl" $
    FcNewtypeDecl
      <$> text
      <* comma
      <*> list tyVar
      <* comma
      <*> text
      <* comma
      <*> tcType
      <* comma
      <*> tcType

foreignCall :: Parser FcForeignCall
foreignCall =
  form "foreign-call" $
    FcForeignCall <$> text <* comma <*> text <* comma <*> foreignSignature

foreignSignature :: Parser FcForeignSignature
foreignSignature =
  form "foreign-signature" $
    FcForeignSignature <$> list foreignType <* comma <*> foreignType <* comma <*> foreignEffect

foreignEffect :: Parser FcForeignEffect
foreignEffect = FcForeignPure <$ keyword "pure" <|> FcForeignRealWorld <$ keyword "real-world"

foreignType :: Parser FcForeignType
foreignType =
  MP.choice
    [ FcForeignInt32 <$ keyword "int32",
      FcForeignInt <$ keyword "int",
      FcForeignWord64 <$ keyword "word64",
      FcForeignAddr <$ keyword "addr"
    ]

bind :: Parser FcBind
bind =
  MP.choice
    [ uncurry FcNonRec <$> form "non-rec" ((,) <$> var <* comma <*> expression),
      FcRec <$> form "rec" (list binding)
    ]

binding :: Parser (Var, FcExpr)
binding = form "binding" ((,) <$> var <* comma <*> expression)

expression :: Parser FcExpr
expression =
  MP.choice
    [ FcVar <$> form "var-expr" var,
      FcLit <$> form "lit" literal,
      uncurry FcApp <$> form "app" ((,) <$> expression <* comma <*> expression),
      uncurry FcTyApp <$> form "type-app-expr" ((,) <$> expression <* comma <*> tcType),
      uncurry FcLam <$> form "lambda" ((,) <$> var <* comma <*> expression),
      uncurry FcTyLam <$> form "type-lambda" ((,) <$> tyVar <* comma <*> expression),
      uncurry FcLet <$> form "let" ((,) <$> bind <* comma <*> expression),
      makeCase <$> form "case" ((,,) <$> expression <* comma <*> var <* comma <*> list alternative),
      uncurry FcCast <$> form "cast" ((,) <$> expression <* comma <*> coercion),
      uncurry FcCallForeign <$> form "call-foreign" ((,) <$> foreignCall <* comma <*> list expression)
    ]
  where
    makeCase (scrutinee, binder, alternatives) = FcCase scrutinee binder alternatives

alternative :: Parser FcAlt
alternative =
  form "alt" $
    FcAlt <$> altConstructor <* comma <*> list var <* comma <*> expression

altConstructor :: Parser FcAltCon
altConstructor =
  MP.choice
    [ DataAlt <$> form "data-alt" text,
      LitAlt <$> form "lit-alt" literal,
      DefaultAlt <$ keyword "default-alt"
    ]

var :: Parser Var
var = form "var" $ do
  name <- text <* comma
  identifier <- unique <* comma
  ty <- tcType <* comma
  resolvedName <- maybeValue text
  pure ((Var name identifier ty) {varResolvedName = resolvedName})

literal :: Parser Literal
literal =
  MP.choice
    [ uncurry LitInt <$> form "int-literal" ((,) <$> runtimeRep <* comma <*> integer),
      uncurry LitChar <$> form "char-literal" ((,) <$> runtimeRep <* comma <*> char),
      LitString <$> form "string-literal" text,
      LitAddr . BS.pack <$> form "addr-literal" (list word8)
    ]

tcType :: Parser TcType
tcType =
  MP.choice
    [ TcTyVar <$> form "type-var" tyVar,
      TcMetaTv <$> form "meta-type" unique,
      uncurry TcTyCon <$> form "type-con" ((,) <$> tyCon <* comma <*> list tcType),
      uncurry TcFunTy <$> form "function-type" ((,) <$> tcType <* comma <*> tcType),
      uncurry TcForAllTy <$> form "forall-type" ((,) <$> tyVar <* comma <*> tcType),
      uncurry TcQualTy <$> form "qualified-type" ((,) <$> list predType <* comma <*> tcType),
      uncurry TcAppTy <$> form "type-app" ((,) <$> tcType <* comma <*> tcType)
    ]

predType :: Parser Pred
predType =
  MP.choice
    [ uncurry ClassPred <$> form "class-pred" ((,) <$> text <* comma <*> list tcType),
      uncurry EqPred <$> form "equality-pred" ((,) <$> tcType <* comma <*> tcType)
    ]

tyVar :: Parser TyVarId
tyVar = form "ty-var" $ do
  name <- text <* comma
  identifier <- unique <* comma
  kind <- kindType
  pure (setTyVarKind kind (TyVarId name identifier))

tyCon :: Parser TyCon
tyCon = form "ty-con" $ do
  name <- text <* comma
  arity <- int <* comma
  kind <- kindType
  pure (setTyConKind kind (mkTyCon name arity kind))

kindType :: Parser Kind
kindType =
  MP.choice
    [ KTYPE <$> form "type-kind" runtimeRep,
      KConstraint <$ keyword "constraint-kind",
      KRuntimeRep <$ keyword "runtime-rep-kind",
      KLevity <$ keyword "levity-kind",
      KVecCount <$ keyword "vec-count-kind",
      KVecElem <$ keyword "vec-elem-kind",
      uncurry KFun <$> form "kind-function" ((,) <$> kindType <* comma <*> kindType),
      KMeta <$> form "meta-kind" unique
    ]

runtimeRep :: Parser RuntimeRep
runtimeRep =
  MP.choice
    [ uncurry VecRep <$> form "vec-rep" ((,) <$> vecCount <* comma <*> vecElem),
      TupleRep <$> form "tuple-rep" (list runtimeRep),
      SumRep <$> form "sum-rep" (list runtimeRep),
      BoxedRep <$> form "boxed-rep" levity,
      Int8Rep <$ keyword "int8-rep",
      Int16Rep <$ keyword "int16-rep",
      Int32Rep <$ keyword "int32-rep",
      Int64Rep <$ keyword "int64-rep",
      IntRep <$ keyword "int-rep",
      Word8Rep <$ keyword "word8-rep",
      Word16Rep <$ keyword "word16-rep",
      Word32Rep <$ keyword "word32-rep",
      Word64Rep <$ keyword "word64-rep",
      WordRep <$ keyword "word-rep",
      AddrRep <$ keyword "addr-rep",
      FloatRep <$ keyword "float-rep",
      DoubleRep <$ keyword "double-rep",
      RuntimeRepVar <$> form "runtime-rep-var" unique,
      RuntimeRepMeta <$> form "runtime-rep-meta" unique
    ]

levity :: Parser Levity
levity = Lifted <$ keyword "lifted" <|> Unlifted <$ keyword "unlifted"

vecCount :: Parser VecCount
vecCount =
  MP.choice
    [ Vec16 <$ keyword "vec16",
      Vec32 <$ keyword "vec32",
      Vec64 <$ keyword "vec64",
      Vec2 <$ keyword "vec2",
      Vec4 <$ keyword "vec4",
      Vec8 <$ keyword "vec8"
    ]

vecElem :: Parser VecElem
vecElem =
  MP.choice
    [ Int8ElemRep <$ keyword "int8-elem-rep",
      Int16ElemRep <$ keyword "int16-elem-rep",
      Int32ElemRep <$ keyword "int32-elem-rep",
      Int64ElemRep <$ keyword "int64-elem-rep",
      Word8ElemRep <$ keyword "word8-elem-rep",
      Word16ElemRep <$ keyword "word16-elem-rep",
      Word32ElemRep <$ keyword "word32-elem-rep",
      Word64ElemRep <$ keyword "word64-elem-rep",
      FloatElemRep <$ keyword "float-elem-rep",
      DoubleElemRep <$ keyword "double-elem-rep"
    ]

coercion :: Parser Coercion
coercion =
  MP.choice
    [ CoVar . EvVar <$> form "co-var" unique,
      Refl <$> form "refl" tcType,
      Sym <$> form "sym" coercion,
      uncurry Trans <$> form "trans" ((,) <$> coercion <* comma <*> coercion),
      uncurry TyConAppCo <$> form "ty-con-app-co" ((,) <$> tyCon <* comma <*> list coercion),
      uncurry AxiomInstCo <$> form "axiom-inst-co" ((,) <$> text <* comma <*> list tcType)
    ]

form :: Text -> Parser a -> Parser a
form name contents = keyword name *> between "(" ")" contents

list :: Parser a -> Parser [a]
list parser = between "[" "]" (parser `MP.sepBy` comma)

maybeValue :: Parser a -> Parser (Maybe a)
maybeValue parser = Nothing <$ keyword "none" <|> Just <$> form "some" parser

between :: Text -> Text -> Parser a -> Parser a
between open close = MP.between (symbol open) (symbol close)

comma :: Parser Text
comma = symbol ","

text :: Parser Text
text = T.pack <$> haskellLiteral "string" '"'

char :: Parser Char
char = haskellLiteral "character" '\''

haskellLiteral :: (Read value) => String -> Char -> Parser value
haskellLiteral description delimiter = do
  source <- lexeme $ do
    contents <- MPC.char delimiter *> MP.many literalPiece <* MPC.char delimiter
    pure (delimiter : concat contents <> [delimiter])
  case readMaybe source of
    Just value -> pure value
    Nothing -> fail ("invalid Haskell " <> description <> " literal")
  where
    literalPiece =
      (\escaped -> ['\\', escaped]) <$> (MPC.char '\\' *> MP.anySingle)
        <|> (: []) <$> MP.satisfy (\character -> character /= delimiter && character /= '\\')

unique :: Parser Unique
unique = Unique <$> int

word8 :: Parser Word8
word8 = do
  value <- integer
  guard (value >= 0 && value <= 255)
  pure (fromInteger value)

int :: Parser Int
int = lexeme (L.signed space L.decimal)

integer :: Parser Integer
integer = lexeme (L.signed space L.decimal)

keyword :: Text -> Parser Text
keyword value = lexeme (MP.try (MPC.string value <* MP.notFollowedBy nameCharacter))

nameCharacter :: Parser Char
nameCharacter = MP.satisfy (\character -> isAlphaNum character || character == '-')

symbol :: Text -> Parser Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

space :: Parser ()
space = L.space MPC.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")
