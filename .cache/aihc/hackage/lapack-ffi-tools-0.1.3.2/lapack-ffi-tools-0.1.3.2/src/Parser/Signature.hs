module Parser.Signature (
   Identifier(..), Direction(..), ParamDecl(..), header3_3, header3_4,
   ) where

import qualified Parser.Combinator as Parser
import qualified Type

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec (Parser, (<|>))
import Text.Printf (printf)

import Control.Monad.HT (void)
import Control.Monad (liftM2, liftM3)
import Control.Applicative ((<$>), (<$))

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Function (fix)
import Data.Maybe (catMaybes)
import Data.Map (Map)


newtype Identifier = Identifier String

subroutine :: Parser (Identifier, [String], Maybe Type.Mono)
subroutine = do
   Parser.spaces
   Parsec.optional $ Parsec.try $ Parser.keyword "RECURSIVE"
   returnType <-
      ((Nothing <$) $ Parsec.try $ Parser.keyword "SUBROUTINE")
      <|>
      (Parser.lexeme (Just <$> typeIdent)
       `Parser.followedBy`
       Parser.keyword "FUNCTION")
   liftM3 (,,)
      (Identifier <$> Parser.identifier)
      (Parser.parens $ Parser.commaSep Parser.identifier)
      (return returnType)


type Mapping = Type.Mapping String Type.Mono

data ParamDecl = Type Type.Mono [(String, Maybe [String])] | External [String]

typeIdent :: Parser Type.Mono
typeIdent =
   (Type.Logical <$ Parsec.string "LOGICAL")
   <|>
   (Type.Character <$
      Parsec.try
         (Parsec.string "CHARACTER" >> Parsec.optional (Parsec.string "*1")))
   <|>
   (Type.Integer <$ Parsec.string "INTEGER")
   <|>
   (Type.RealSingle <$ Parsec.string "REAL")
   <|>
   (Parser.lexeme (Parsec.string "DOUBLE") >>
      ((Type.RealDouble <$ Parsec.string "PRECISION")
       <|>
       (Type.ComplexDouble <$ Parsec.string "COMPLEX")))
   <|>
   (Parsec.string "COMPLEX" >>
      ((Type.ComplexDouble <$ Parsec.string "*16")
       <|>
       return Type.ComplexSingle))

expression :: Parser String
expression =
   fmap concat $
   Parsec.many1 $
      Parsec.many1 Parsec.alphaNum
      <|>
      fmap return (Parsec.oneOf "+-*/: ")
      <|>
      (fmap (printf "(%s)") $
       Parsec.between
         (Parser.lexeme $ Parsec.char '(') (Parser.lexeme $ Parsec.char ')')
         (List.intercalate "," <$> Parser.commaSep1 expression))

externalDecl :: Parser [String]
externalDecl =
   Parsec.try (Parsec.string "EXTERNAL")
   >>
   Parser.spaces
   >>
   Parser.commaSep1 Parser.identifier

parameterDecl :: Parser ParamDecl
parameterDecl =
   Parsec.spaces >>
   (fmap External externalDecl
    <|>
    (liftM2 Type
      (Parser.lexeme typeIdent)
      (Parser.commaSep $
       liftM2 (,)
         Parser.identifier
         (Parsec.option Nothing $
          fmap Just $ Parser.parens $ Parser.commaSep1 $
          Parser.lexeme (Parsec.string "*") <|> expression))))

data Direction = Input | Output | InputOutput | InputWorkspace | Workspace
   deriving (Eq, Show)

direction3_3 :: Parser Direction
direction3_3 = do
   dirs <- Parsec.many1 (Parsec.letter <|> Parsec.char '/' <|> Parsec.char ' ')
   case dirs of
      "input" -> return Input
      "input or output" -> return InputOutput
      "input/output" -> return InputOutput
      "input/workspace" -> return InputWorkspace
      "output" -> return Output
      "workspace" -> return Workspace
      "workspace/output" -> return Workspace
      "input/workspace/output" -> return InputOutput
      _ -> fail $ "unknown direction specification: " ++ dirs

arrayDimensions :: String -> Parser (Maybe [String])
arrayDimensions commentStart = do
   Parser.keyword "array,"
   (do
      Parser.keyword "dimension"
      (do
         dims <- Parser.parens (Parser.commaSep1 expression)
         (Nothing <$
            Parsec.try
               (Parser.keyword "if" <|>
                (Parser.keyword ('\n':commentStart) >> Parser.keyword "if")))
          <|> return (Just dims))
       <|> return Nothing)
    <|>
    return Nothing

optionalDimensions :: String -> Parser (Maybe Mapping)
optionalDimensions commentStart =
   (fmap Type.Array <$> arrayDimensions commentStart)
   <|>
   return (Just Type.Scalar)

{-
LOGICAL FUNCTION of one COMPLEX argument
-}
callback :: Parser (Int, Type.Mono, Type.Mono)
callback = do
   returnType <- Parser.lexeme typeIdent
   Parser.keyword "FUNCTION"
   Parser.keyword "of"
   let arguments n nStr argStr = do
         Parsec.try $ Parser.keyword nStr
         typ <- Parser.lexeme typeIdent
         Parser.keyword argStr
         return (n,typ)
   (n,argType) <-
      arguments 1 "one" "argument" <|>
      arguments 2 "two" "arguments" <|>
      arguments 3 "three" "arguments"
   return (n,argType,returnType)

parameterComment3_3 :: Parser (String, (Direction, Type.Mono, Maybe Mapping))
parameterComment3_3 = do
   void $ Parser.lexeme $ Parsec.char '*'
   liftM2 (,) Parser.identifier $
      liftM3 (,,)
         (Parser.parens direction3_3) (Parser.lexeme typeIdent)
         (optionalDimensions "*")
      `Parser.followedBy`
      Parser.garbage

anyComment :: Parser ()
anyComment = do
   void $ Parsec.char '*'
   Parser.garbage

commentOrBlank :: Parser ()
commentOrBlank = Parser.oneLine $ anyComment <|> return ()


header3_3 ::
   Parser
      ((Identifier, [String], Maybe Type.Mono), [ParamDecl],
       Map String (Direction, Type.Mono, Maybe Mapping))
header3_3 = do
   prototype <- Parser.oneLine subroutine
   Parsec.skipMany commentOrBlank
   Parsec.optional $ Parsec.try $ Parser.oneLine $ do
      Parser.spaces
      Parser.keyword "IMPLICIT"
      Parser.keyword "NONE"
   Parsec.skipMany commentOrBlank
   paramTypes <-
      fix $ \loop ->
         liftM2 (:) (Parser.oneLine parameterDecl) $
         fix $ \loopCmt ->
            ([] <$ Parsec.try (Parsec.lookAhead parameterComment3_3))
            <|>
            (commentOrBlank >> loopCmt)
            <|>
            loop
   paramDirs <-
      Parsec.many $
         (Just <$> Parsec.try (Parser.oneLine parameterComment3_3))
         <|>
         (Nothing <$ commentOrBlank)
   return (prototype, paramTypes, Map.fromList $ catMaybes paramDirs)


direction3_4 :: Parser (Bool -> Direction)
direction3_4 = do
   dirs <- Parsec.many1 (Parsec.letter <|> Parsec.char ',')
   case dirs of
      "in" -> return $ \work -> if work then InputWorkspace else Input
      "in,out" -> return $ const InputOutput
      "out" -> return $ \work -> if work then Workspace else Output
      _ -> fail $ "unknown direction specification: " ++ dirs

{-
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
-}
parameterComment3_4 :: Parser (String, (Direction, Type.Mono, Maybe Mapping))
parameterComment3_4 = do
   let doxyCmt = void $ Parser.lexeme $ Parsec.string "*>"
   void $ Parsec.try $ doxyCmt >> Parsec.string "\\param"
   dirFromWork <-
      Parser.lexeme $
      Parsec.between (Parsec.char '[') (Parsec.char ']') direction3_4
   name <- Parser.identifier
   void $ Parsec.newline
   let dir = dirFromWork $ List.isSuffixOf "WORK" name

   doxyCmt >> Parsec.string "\\verbatim" >> void Parsec.newline

   doxyCmt
   void $ Parser.lexeme $ Parsec.string name
   void $ Parser.lexeme $ Parsec.string "is"

   (typ,dims) <-
      Parser.oneLine $
         (do
            Parsec.try $ Parser.keyword "a"
            (n,argType,returnType) <- callback
            return (returnType, Just $ Type.Function n argType))
         <|>
         (liftM2 (,) (Parser.lexeme typeIdent) (optionalDimensions "*>")
          `Parser.followedBy`
          Parser.garbage)

   return (name, (dir,typ,dims))


header3_4 ::
   Parser
      ((Identifier, [String], Maybe Type.Mono), [ParamDecl],
       Map String (Direction, Type.Mono, Maybe Mapping))
header3_4 = do
   void $ fix $ \loop ->
      (Parsec.try $ Parser.oneLine $
         Parser.lexeme (Parsec.string "*") >> Parsec.string "Arguments:")
      <|>
      (commentOrBlank >> loop)
   void $ Parser.oneLine $
      Parser.lexeme (Parsec.string "*") >> Parsec.string "=========="
   void $ Parser.oneLine $ Parser.lexeme (Parsec.string "*")
   paramDirs <-
      Parsec.many $
         (Just <$> parameterComment3_4)
         <|>
         (Nothing <$
          Parsec.try (Parser.oneLine (Parsec.string "*>" >> Parser.garbage)))
   Parsec.skipMany commentOrBlank
   prototype <- Parser.oneLine subroutine
   Parsec.skipMany commentOrBlank
   Parsec.optional $ Parsec.try $ Parser.oneLine $ do
      Parser.spaces
      Parser.keyword "IMPLICIT" <|> Parser.keyword "implicit"
      Parser.keyword "NONE" <|> Parser.keyword "none"
   Parsec.skipMany commentOrBlank
   paramTypes <-
      fix $ \loop ->
         liftM2 (:) (Parser.oneLine parameterDecl) $
         fix $ \loopCmt ->
            (Parsec.try $ Parser.oneLine $
               [] <$ (Parser.lexeme (Parsec.string "*") >>
                      Parsec.string "==================" >> Parser.garbage))
            <|>
            (commentOrBlank >> loopCmt)
            <|>
            loop
   return (prototype, paramTypes, Map.fromList $ catMaybes paramDirs)
