{-# LANGUAGE OverloadedStrings #-}

-- | Convert the result of a splice to the surface syntax.
--
-- The input is the tree of "Aihc.Th.Wire". Its constructor names are the
-- names of the Template Haskell syntax tree, such as @AppE@ and @LitE@.
-- The output is the syntax that the parser gives, so name resolution and
-- the type checker handle the result of a splice as they handle source
-- text.
--
-- The conversion handles a subset of the expressions, the declarations,
-- the types, and the patterns. For a construct outside the subset, the
-- result is a message that names the construct.
module Aihc.Th.Convert
  ( expressionFromTree,
    typeResultFromTree,
    declarationsFromTree,
  )
where

import Aihc.Parser.Parens (addDeclParens, addExprParens, addTypeParens)
import Aihc.Parser.Syntax
import Aihc.Resolve.Types (ExactTerm (..), ExactType (..), GlobalName (..), PackageId (..), ResolutionNamespace (..))
import Aihc.Th.Wire (Tree (..))
import Data.Char (isAlpha, isUpper)
import Data.Text (Text)
import Data.Text qualified as T

-- | The expression of an @Exp@ tree.
expressionFromTree :: Tree -> Either Text Expr
expressionFromTree = fmap addExprParens . expression

-- | The type of a @Type@ tree, the result of a type splice.
typeResultFromTree :: Tree -> Either Text Type
typeResultFromTree = fmap addTypeParens . typeFromTree

-- | The declarations of a list of @Dec@ trees.
declarationsFromTree :: Tree -> Either Text [Decl]
declarationsFromTree tree = map addDeclParens <$> declarations tree

-- | A pragma, such as @INLINE@ or @SPECIALISE@, does not change the
-- meaning of the declarations. The conversion leaves it out.
declarations :: Tree -> Either Text [Decl]
declarations tree = list tree >>= mapM declaration . filter (not . isPragma)
  where
    isPragma item =
      case item of
        TreeNode "PragmaD" _ -> True
        _ -> False

declaration :: Tree -> Either Text Decl
declaration tree =
  case tree of
    TreeNode "SigD" [name, signature] -> DeclTypeSig . pure <$> binderFromTree name <*> typeFromTree signature
    TreeNode "FunD" [name, clauses] -> DeclValue <$> (FunctionBind <$> binderFromTree name <*> (list clauses >>= mapM clause))
    -- A value with a variable pattern is a function with no arguments.
    TreeNode "ValD" [TreeNode "VarP" [name], body, locals] -> do
      binder <- binderFromTree name
      rhs <- rightSide body locals
      pure (DeclValue (FunctionBind binder [Match [] MatchHeadPrefix [] rhs]))
    TreeNode "ValD" [pattern', body, locals] -> DeclValue <$> (PatternBind NoMultiplicityTag <$> patternFromTree pattern' <*> rightSide body locals)
    TreeNode "InstanceD" [TreeNode "Nothing" [], context, instanceHead, members] -> do
      context' <- list context >>= mapM typeFromTree
      instanceHead' <- typeFromTree instanceHead
      items <- list members >>= fmap concat . mapM instanceItem
      pure (DeclInstance (InstanceDecl [] Nothing [] context' instanceHead' items))
    TreeNode "InstanceD" [overlap, _, _, _] -> unsupported "overlap pragma" overlap
    _ -> unsupported "declaration" tree
  where
    -- A pragma in an instance, such as @INLINE@, does not change the
    -- meaning of the instance. The conversion leaves it out.
    instanceItem item =
      case item of
        TreeNode "SigD" [name, signature] -> (\binder signature' -> [InstanceItemTypeSig [binder] signature']) <$> binderFromTree name <*> typeFromTree signature
        TreeNode "PragmaD" _ -> Right []
        _ -> do
          member <- declaration item
          case member of
            DeclValue binding -> Right [InstanceItemBind binding]
            _ -> unsupported "instance member" item
    clause clauseTree =
      case clauseTree of
        TreeNode "Clause" [patterns, body, locals] -> Match [] MatchHeadPrefix <$> (list patterns >>= mapM patternFromTree) <*> rightSide body locals
        _ -> unsupported "clause" clauseTree

-- | The right side of a binding or of a case alternative, from its body
-- and its local declarations.
rightSide :: Tree -> Tree -> Either Text (Rhs Expr)
rightSide body locals = do
  locals' <- declarations locals
  let localDecls = if null locals' then Nothing else Just locals'
  case body of
    TreeNode "NormalB" [value] -> (\value' -> UnguardedRhs [] value' localDecls) <$> expression value
    TreeNode "GuardedB" [guarded] -> (\branches -> GuardedRhss [] branches localDecls) <$> (list guarded >>= mapM branch)
    _ -> unsupported "body" body
  where
    branch tree =
      case tree of
        TreeNode "(,)" [TreeNode "NormalG" [condition], value] -> (\condition' value' -> GuardedRhs [] [GuardExpr condition'] value') <$> expression condition <*> expression value
        _ -> unsupported "guard" tree

-- | The type of a @Type@ tree. An application of the arrow, the list, or a
-- tuple constructor to all its arguments becomes the syntax of that type.
typeFromTree :: Tree -> Either Text Type
typeFromTree tree =
  case typeApplication tree [] of
    (TreeNode "ArrowT" [], [argument, result]) -> TFun ArrowUnrestricted <$> typeFromTree argument <*> typeFromTree result
    (TreeNode "ListT" [], [element]) -> (\element' -> TList Unpromoted [element']) <$> typeFromTree element
    (TreeNode "TupleT" [TreeInteger size], fields)
      | toInteger (length fields) == size -> TTuple Boxed Unpromoted <$> mapM typeFromTree fields
    (TreeNode "ConT" [name], arguments) -> foldl TApp . (`TCon` Unpromoted) <$> nameFromTree name <*> mapM typeFromTree arguments
    (TreeNode "VarT" [name], arguments) -> foldl TApp . TVar <$> binderFromTree name <*> mapM typeFromTree arguments
    (TreeNode "PromotedT" [name], arguments) -> foldl TApp . (`TCon` Promoted) <$> nameFromTree name <*> mapM typeFromTree arguments
    (TreeNode "ParensT" [inner], arguments) -> foldl TApp . TParen <$> typeFromTree inner <*> mapM typeFromTree arguments
    (TreeNode "SigT" [inner, kind], arguments) -> foldl TApp . TParen <$> (TKindSig <$> typeFromTree inner <*> typeFromTree kind) <*> mapM typeFromTree arguments
    (TreeNode "LitT" [value], []) -> TTypeLit <$> typeLiteral value
    (TreeNode "ForallT" [binders, context, body], []) -> do
      binders' <- list binders >>= mapM typeVariableBinder
      context' <- list context >>= mapM typeFromTree
      body' <- typeFromTree body
      let qualified = if null context' then body' else TContext context' body'
      pure (if null binders' then qualified else TForall (ForallTelescope ForallInvisible binders') qualified)
    (TreeNode "ForallVisT" [binders, body], []) -> TForall . ForallTelescope ForallVisible <$> (list binders >>= mapM typeVariableBinder) <*> typeFromTree body
    (other, _) -> unsupported "type" other
  where
    -- The head of a chain of type applications, and its arguments.
    typeApplication applied arguments =
      case applied of
        TreeNode "AppT" [function, argument] -> typeApplication function (argument : arguments)
        _ -> (applied, arguments)
    typeLiteral value =
      case value of
        TreeNode "NumTyLit" [TreeInteger number] -> Right (TypeLitInteger number (T.pack (show number)))
        TreeNode "StrTyLit" [TreeString string] -> Right (TypeLitSymbol string (T.pack (show (T.unpack string))))
        TreeNode "CharTyLit" [TreeChar char] -> Right (TypeLitChar char (T.pack (show char)))
        _ -> unsupported "type literal" value

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
    TreeNode "CaseE" [scrutinee, matches] -> ECase <$> expression scrutinee <*> (list matches >>= mapM alternative)
    TreeNode "LamCaseE" [matches] -> ELambdaCase <$> (list matches >>= mapM alternative)
    TreeNode "LetE" [locals, body] -> ELetDecls <$> declarations locals <*> expression body
    TreeNode "SigE" [inner, signature] -> ETypeSig <$> expression inner <*> typeFromTree signature
    TreeNode "RecConE" [name, fields] -> (\name' fields' -> ERecordCon name' fields' False) <$> nameFromTree name <*> (list fields >>= mapM field)
    TreeNode "RecUpdE" [record, fields] -> ERecordUpd <$> expression record <*> (list fields >>= mapM field)
    _ -> unsupported "expression" tree
  where
    alternative matchTree =
      case matchTree of
        TreeNode "Match" [pattern', body, locals] -> CaseAlt [] <$> patternFromTree pattern' <*> rightSide body locals
        _ -> unsupported "case alternative" matchTree
    field fieldTree =
      case fieldTree of
        TreeNode "(,)" [name, value] -> (\name' value' -> RecordField name' value' False) <$> nameFromTree name <*> expression value
        _ -> unsupported "record field" fieldTree

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
    -- A primitive literal has its sign in the literal, as @-1#@ has.
    TreeNode "IntPrimL" [TreeInteger number] -> Right (EInt number TIntHash (T.pack (show number) <> "#"))
    TreeNode "WordPrimL" [TreeInteger number]
      | number >= 0 -> Right (EInt number TWordHash (T.pack (show number) <> "##"))
    TreeNode "CharPrimL" [TreeChar char] -> Right (ECharHash char (T.pack (show char) <> "#"))
    _ -> unsupported "literal" tree
  where
    integerLiteral number = EInt number TInteger (T.pack (show number))

patternFromTree :: Tree -> Either Text Pattern
patternFromTree tree =
  case tree of
    TreeNode "VarP" [name] -> PVar <$> binderFromTree name
    TreeNode "WildP" [] -> Right PWildcard
    TreeNode "ConP" [name, typeArguments, fields] -> do
      typeArguments' <- list typeArguments
      if null typeArguments'
        then PCon <$> nameFromTree name <*> pure [] <*> (list fields >>= mapM patternFromTree)
        else unsupported "pattern with type arguments" tree
    TreeNode "TupP" [fields] -> PTuple Boxed <$> (list fields >>= mapM patternFromTree)
    TreeNode "InfixP" [left, name, right] -> PInfix <$> patternFromTree left <*> nameFromTree name <*> patternFromTree right
    TreeNode "ParensP" [inner] -> PParen <$> patternFromTree inner
    TreeNode "AsP" [name, inner] -> PAs <$> binderFromTree name <*> patternFromTree inner
    TreeNode "BangP" [inner] -> PStrict <$> patternFromTree inner
    TreeNode "TildeP" [inner] -> PIrrefutable <$> patternFromTree inner
    TreeNode "ListP" [elements] -> PList <$> (list elements >>= mapM patternFromTree)
    TreeNode "SigP" [inner, signature] -> PTypeSig <$> patternFromTree inner <*> typeFromTree signature
    TreeNode "RecP" [name, fields] -> (\name' fields' -> PRecord name' fields' False) <$> nameFromTree name <*> (list fields >>= mapM field)
    TreeNode "LitP" [TreeNode "IntegerL" [TreeInteger number]]
      | number >= 0 -> Right (PLit (LitInt number TInteger (T.pack (show number))))
    TreeNode "LitP" [TreeNode "CharL" [TreeChar char]] -> Right (PLit (LitChar char (T.pack (show char))))
    TreeNode "LitP" [TreeNode "IntPrimL" [TreeInteger number]] -> Right (PLit (LitInt number TIntHash (T.pack (show number) <> "#")))
    TreeNode "LitP" [TreeNode "WordPrimL" [TreeInteger number]]
      | number >= 0 -> Right (PLit (LitInt number TWordHash (T.pack (show number) <> "##")))
    TreeNode "LitP" [TreeNode "CharPrimL" [TreeChar char]] -> Right (PLit (LitCharHash char (T.pack (show char) <> "#")))
    TreeNode "LitP" [TreeNode "StringL" [TreeString string]] -> Right (PLit (LitString string (T.pack (show (T.unpack string)))))
    _ -> unsupported "pattern" tree
  where
    field fieldTree =
      case fieldTree of
        TreeNode "(,)" [name, value] -> (\name' value' -> RecordField name' value' False) <$> nameFromTree name <*> patternFromTree value
        _ -> unsupported "record field" fieldTree

-- | The binder of a @Name@ tree. A binder has no module.
binderFromTree :: Tree -> Either Text UnqualifiedName
binderFromTree tree = do
  Name {nameQualifier, nameType, nameText} <- nameFromTree tree
  case nameQualifier of
    Nothing -> Right (mkUnqualifiedName nameType nameText)
    Just _ -> Left "A splice result binds a qualified name."

-- | A binder of a type variable: @PlainTV@ or @KindedTV@, with a flag. The
-- flag @InferredSpec@ gives an inferred binder, and each other flag gives
-- a specified binder.
typeVariableBinder :: Tree -> Either Text TyVarBinder
typeVariableBinder tree =
  case tree of
    TreeNode "PlainTV" [name, flag] -> binder name flag Nothing
    TreeNode "KindedTV" [name, flag, kind] -> typeFromTree kind >>= binder name flag . Just
    _ -> unsupported "type variable binder" tree
  where
    binder name flag kind = do
      variable <- binderFromTree name
      pure
        TyVarBinder
          { tyVarBinderAnns = [],
            tyVarBinderName = unqualifiedNameText variable,
            tyVarBinderKind = kind,
            tyVarBinderSpecificity = case flag of
              TreeNode "InferredSpec" [] -> TyVarBInferred
              _ -> TyVarBSpecified,
            tyVarBinderVisibility = TyVarBVisible
          }

-- | The name of a @Name@ tree. A name with no module, a name with a module
-- qualifier, a unique name, and a name that is bound to its package and
-- module are in the subset. The last has an 'ExactTerm' or an 'ExactType'
-- mark, so name resolution gives it that term or type in each scope. A name that Template Haskell made
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
            -- The number of a unique name is part of its text. The text
            -- has a character that a source name cannot have at that
            -- position, so the name is different from each source name.
            TreeNode "NameU" [TreeInteger unique] -> Right (mkName Nothing (occurrenceType occurrence) (occurrence <> "#u" <> T.pack (show unique)))
            TreeNode "NameG" [TreeNode space [], TreeNode "PkgName" [TreeString package], TreeNode "ModName" [TreeString home]]
              | space `elem` ["VarName", "DataName"] ->
                  let global = GlobalName occurrence (PackageId package) home ResolutionNamespaceTerm
                   in Right (mkName Nothing (occurrenceType occurrence) occurrence) {nameAnns = [mkAnnotation (ExactTerm global)]}
              | space == "TcClsName" ->
                  let global = GlobalName occurrence (PackageId package) home ResolutionNamespaceType
                   in Right (mkName Nothing (occurrenceType occurrence) occurrence) {nameAnns = [mkAnnotation (ExactType global)]}
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
