{-# LANGUAGE OverloadedStrings #-}

-- | Convert the result of a splice to the surface syntax.
--
-- The input is the tree of "Aihc.Th.Wire". Its constructor names are the
-- names of the Template Haskell syntax tree, such as @AppE@ and @LitE@.
-- The output is the syntax that the parser gives, so name resolution and
-- the type checker handle the result of a splice as they handle source
-- text.
--
-- The conversion handles a subset of the expressions. For a construct
-- outside the subset, the result is a message that names the construct.
module Aihc.Th.Convert
  ( expressionFromTree,
  )
where

import Aihc.Parser.Parens (addExprParens)
import Aihc.Parser.Syntax
import Aihc.Resolve.Types (ExactTerm (..), GlobalName (..), PackageId (..), ResolutionNamespace (..))
import Aihc.Th.Wire (Tree (..))
import Data.Char (isAlpha, isUpper)
import Data.Text (Text)
import Data.Text qualified as T

-- | The expression of an @Exp@ tree.
expressionFromTree :: Tree -> Either Text Expr
expressionFromTree = fmap addExprParens . expression

expression :: Tree -> Either Text Expr
expression tree =
  case tree of
    TreeNode "VarE" [name] -> EVar <$> nameFromTree name
    TreeNode "ConE" [name] -> EVar <$> nameFromTree name
    TreeNode "UnboundVarE" [name] -> EVar <$> nameFromTree name
    TreeNode "LitE" [value] -> literal value
    TreeNode "AppE" [function, argument] -> EApp <$> expression function <*> expression argument
    TreeNode "InfixE" [left, operator, right] -> do
      name <- operatorName operator
      leftOperand <- optional operand left
      rightOperand <- optional operand right
      pure $ case (leftOperand, rightOperand) of
        (Just first, Just second) -> EInfix first name second
        (Just first, Nothing) -> ESectionL first name
        (Nothing, Just second) -> ESectionR name second
        (Nothing, Nothing) -> EVar name
    TreeNode "ParensE" [inner] -> EParen <$> expression inner
    TreeNode "LamE" [patterns, body] -> ELambdaPats <$> (list patterns >>= mapM patternFromTree) <*> expression body
    TreeNode "TupE" [fields] -> ETuple Boxed <$> (list fields >>= mapM (optional expression))
    TreeNode "CondE" [condition, whenTrue, whenFalse] -> EIf <$> expression condition <*> expression whenTrue <*> expression whenFalse
    TreeNode "ListE" [elements] -> EList <$> (list elements >>= mapM expression)
    _ -> unsupported "expression" tree

-- | An operand of an infix expression. Template Haskell gives the operands
-- of @InfixE@ as they are, with no fixity. The parentheses keep the name
-- resolution from a change to the structure.
operand :: Tree -> Either Text Expr
operand tree = do
  inner <- expression tree
  pure $ case inner of
    EInfix {} -> EParen inner
    _ -> inner

operatorName :: Tree -> Either Text Name
operatorName tree =
  case tree of
    TreeNode "VarE" [name] -> nameFromTree name
    TreeNode "ConE" [name] -> nameFromTree name
    TreeNode "UnboundVarE" [name] -> nameFromTree name
    _ -> unsupported "operator" tree

literal :: Tree -> Either Text Expr
literal tree =
  case tree of
    TreeNode "IntegerL" [TreeInteger number]
      | number < 0 -> Right (EParen (ENegate (integerLiteral (negate number))))
      | otherwise -> Right (integerLiteral number)
    TreeNode "CharL" [TreeChar char] -> Right (EChar char (T.pack (show char)))
    TreeNode "StringL" [TreeString string] -> Right (EString string (T.pack (show (T.unpack string))))
    _ -> unsupported "literal" tree
  where
    integerLiteral number = EInt number TInteger (T.pack (show number))

patternFromTree :: Tree -> Either Text Pattern
patternFromTree tree =
  case tree of
    TreeNode "VarP" [name] -> do
      Name {nameQualifier, nameType, nameText} <- nameFromTree name
      case nameQualifier of
        Nothing -> Right (PVar (mkUnqualifiedName nameType nameText))
        Just _ -> Left "A splice result binds a qualified name."
    TreeNode "WildP" [] -> Right PWildcard
    _ -> unsupported "pattern" tree

-- | The name of a @Name@ tree. A name with no module, a name with a module
-- qualifier, and a term name that is bound to its package and module are
-- in the subset. The last has an 'ExactTerm' mark, so name resolution
-- gives it that term in each scope. A name that Template Haskell made
-- unique is not in the subset.
nameFromTree :: Tree -> Either Text Name
nameFromTree tree =
  case tree of
    TreeNode "Name" [TreeNode "OccName" [TreeString occurrence], flavour]
      | T.null occurrence -> Left "A splice result has a name with no text."
      | otherwise ->
          case flavour of
            TreeNode "NameS" [] -> Right (mkName Nothing (occurrenceType occurrence) occurrence)
            TreeNode "NameQ" [TreeNode "ModName" [TreeString qualifier]] -> Right (mkName (Just qualifier) (occurrenceType occurrence) occurrence)
            TreeNode "NameG" [TreeNode space [], TreeNode "PkgName" [TreeString package], TreeNode "ModName" [TreeString home]]
              | space `elem` ["VarName", "DataName"] ->
                  let global = GlobalName occurrence (PackageId package) home ResolutionNamespaceTerm
                   in Right (mkName Nothing (occurrenceType occurrence) occurrence) {nameAnns = [mkAnnotation (ExactTerm global)]}
            _ -> unsupported "name flavour" flavour
    _ -> unsupported "name" tree

-- | The syntactic category of a name, from its first character.
occurrenceType :: Text -> NameType
occurrenceType occurrence =
  case T.uncons occurrence of
    Just (first, _)
      | isUpper first -> NameConId
      | isAlpha first || first == '_' -> NameVarId
      | first == ':' -> NameConSym
    _ -> NameVarSym

-- | The value of a @Maybe@ tree.
optional :: (Tree -> Either Text value) -> Tree -> Either Text (Maybe value)
optional convert tree =
  case tree of
    TreeNode "Nothing" [] -> Right Nothing
    TreeNode "Just" [value] -> Just <$> convert value
    _ -> unsupported "optional value" tree

-- | The elements of a list tree.
list :: Tree -> Either Text [Tree]
list tree =
  case tree of
    TreeNode "[]" [] -> Right []
    TreeNode "(:)" [first, rest] -> (first :) <$> list rest
    _ -> unsupported "list" tree

unsupported :: Text -> Tree -> Either Text value
unsupported kind tree =
  Left ("The compiler does not support the " <> kind <> " " <> constructor <> " in a splice result.")
  where
    constructor =
      case tree of
        TreeNode name _ -> name
        TreeInteger _ -> "integer"
        TreeChar _ -> "character"
        TreeString _ -> "string"
        TreeRatio _ -> "ratio"
