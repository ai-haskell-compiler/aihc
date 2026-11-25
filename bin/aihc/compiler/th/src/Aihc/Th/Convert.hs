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
    nameFromTree,
    typeResultFromTree,
    patternResultFromTree,
    declarationsFromTree,
  )
where

import Aihc.Parser.Parens (addDeclParens, addExprParens, addPatternParens, addTypeParens)
import Aihc.Parser.Syntax
import Aihc.Resolve.Types (ExactTerm (..), ExactType (..), GlobalName (..), PackageId (..), ResolutionNamespace (..))
import Aihc.Th.Wire (Tree (..))
import Control.Monad ((>=>))
import Data.Char (isAlpha, isUpper)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text)
import Data.Text qualified as T

-- | The expression of an @Exp@ tree.
expressionFromTree :: Tree -> Either Text Expr
expressionFromTree = fmap addExprParens . expression

-- | The type of a @Type@ tree, the result of a type splice.
typeResultFromTree :: Tree -> Either Text Type
typeResultFromTree = fmap addTypeParens . typeFromTree

-- | The pattern of a @Pat@ tree, for the position of a pattern splice.
patternResultFromTree :: Tree -> Either Text Pattern
patternResultFromTree = fmap addPatternParens . patternFromTree

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
    TreeNode "TySynInstD" [equation] -> DeclTypeFamilyInst <$> typeFamilyInstance equation
    TreeNode "TySynD" [name, binders, body] -> do
      head' <- PrefixBinderHead <$> binderFromTree name <*> (list binders >>= mapM typeVariableBinder)
      DeclTypeSyn . TypeSynDecl head' <$> typeFromTree body
    TreeNode "DataD" [context, name, binders, kind, constructors, clauses] -> do
      context' <- list context >>= mapM typeFromTree
      head' <- PrefixBinderHead <$> binderFromTree name <*> (list binders >>= mapM typeVariableBinder)
      kind' <- optional typeFromTree kind
      constructors' <- list constructors >>= mapM dataConstructor
      clauses' <- list clauses >>= mapM derivingClause
      pure (DeclData (DataDecl Nothing head' context' kind' constructors' clauses'))
    TreeNode "NewtypeD" [context, name, binders, kind, constructor', clauses] -> do
      context' <- list context >>= mapM typeFromTree
      head' <- PrefixBinderHead <$> binderFromTree name <*> (list binders >>= mapM typeVariableBinder)
      kind' <- optional typeFromTree kind
      constructor'' <- dataConstructor constructor'
      clauses' <- list clauses >>= mapM derivingClause
      pure (DeclNewtype (NewtypeDecl Nothing head' context' kind' (Just constructor'') clauses'))
    TreeNode "DataInstD" _ -> DeclDataFamilyInst <$> dataFamilyInstance tree
    TreeNode "NewtypeInstD" _ -> DeclDataFamilyInst <$> dataFamilyInstance tree
    TreeNode "InstanceD" [TreeNode "Nothing" [], context, instanceHead, members] -> do
      context' <- list context >>= mapM typeFromTree
      instanceHead' <- typeFromTree instanceHead
      items <- list members >>= fmap concat . mapM instanceItem
      pure (DeclInstance (InstanceDecl [] Nothing [] context' instanceHead' items))
    TreeNode "InstanceD" [overlap, _, _, _] -> unsupported "overlap pragma" overlap
    TreeNode "StandaloneDerivD" [strategy, context, instanceHead] -> do
      strategy' <- optional strategyFromTree strategy
      context' <- list context >>= mapM typeFromTree
      instanceHead' <- typeFromTree instanceHead
      pure (DeclStandaloneDeriving (StandaloneDerivingDecl strategy' [] Nothing [] context' instanceHead'))
    TreeNode "ClassD" [context, name, binders, dependencies, members] -> do
      context' <- list context >>= mapM typeFromTree
      className <- binderFromTree name
      binders' <- list binders >>= mapM typeVariableBinder
      dependencies' <- list dependencies >>= mapM functionalDependency
      items <- list members >>= fmap concat . mapM classItem
      pure (DeclClass (ClassDecl (if null context' then Nothing else Just context') (PrefixBinderHead className binders') dependencies' items))
    TreeNode "ForeignD" [foreign'] -> DeclForeign <$> foreignDeclaration foreign'
    _ -> unsupported "declaration" tree
  where
    -- A pragma in an instance, such as @INLINE@, does not change the
    -- meaning of the instance. The conversion leaves it out.
    instanceItem item =
      case item of
        TreeNode "SigD" [name, signature] -> (\binder signature' -> [InstanceItemTypeSig [binder] signature']) <$> binderFromTree name <*> typeFromTree signature
        TreeNode "PragmaD" _ -> Right []
        TreeNode "TySynInstD" [equation] -> pure . InstanceItemTypeFamilyInst <$> typeFamilyInstance equation
        TreeNode "DataInstD" _ -> pure . InstanceItemDataFamilyInst <$> dataFamilyInstance item
        TreeNode "NewtypeInstD" _ -> pure . InstanceItemDataFamilyInst <$> dataFamilyInstance item
        _ -> do
          member <- declaration item
          case member of
            DeclValue binding -> Right [InstanceItemBind binding]
            _ -> unsupported "instance member" item
    -- A pragma in a class, such as @MINIMAL@, does not change the meaning
    -- of the class. The conversion leaves it out.
    classItem item =
      case item of
        TreeNode "SigD" [name, signature] -> (\binder signature' -> [ClassItemTypeSig [binder] signature']) <$> binderFromTree name <*> typeFromTree signature
        TreeNode "DefaultSigD" [name, signature] -> (\binder signature' -> [ClassItemDefaultSig binder signature']) <$> binderFromTree name <*> typeFromTree signature
        TreeNode "PragmaD" _ -> Right []
        _ -> do
          member <- declaration item
          case member of
            DeclValue binding -> Right [ClassItemDefault binding]
            _ -> unsupported "class member" item
    functionalDependency dependency =
      case dependency of
        TreeNode "FunDep" [determiners, determined] -> do
          determiners' <- list determiners >>= mapM variableText
          determined' <- list determined >>= mapM variableText
          pure (FunctionalDependency [] determiners' determined')
        _ -> unsupported "functional dependency" dependency
    variableText = fmap unqualifiedNameText . binderFromTree
    -- The left side of an equation is the family applied to its arguments.
    -- The binders of an explicit @forall@ come first.
    typeFamilyInstance equation =
      case equation of
        TreeNode "TySynEqn" [binders, left, right] -> do
          binders' <- optional (list >=> mapM typeVariableBinder) binders
          left' <- typeFromTree left
          right' <- typeFromTree right
          pure (TypeFamilyInst (fromMaybe [] binders') TypeHeadPrefix left' right')
        _ -> unsupported "type family equation" equation
    -- A data family instance has no context in the surface syntax. GHC
    -- also does not accept a context there.
    dataFamilyInstance instanceTree =
      case instanceTree of
        TreeNode "DataInstD" [context, binders, left, kind, constructors, clauses] -> do
          list context >>= noContext
          familyInstance False binders left kind (list constructors >>= mapM dataConstructor) clauses
        TreeNode "NewtypeInstD" [context, binders, left, kind, constructor', clauses] -> do
          list context >>= noContext
          familyInstance True binders left kind (pure <$> dataConstructor constructor') clauses
        _ -> unsupported "data family instance" instanceTree
      where
        noContext context =
          case context of
            [] -> Right ()
            first : _ -> unsupported "data family instance context" first
        familyInstance isNewtype binders left kind constructors clauses = do
          binders' <- optional (list >=> mapM typeVariableBinder) binders
          left' <- typeFromTree left
          kind' <- optional typeFromTree kind
          constructors' <- constructors
          clauses' <- list clauses >>= mapM derivingClause
          pure (DataFamilyInst isNewtype (fromMaybe [] binders') left' kind' constructors' clauses')
    clause clauseTree =
      case clauseTree of
        TreeNode "Clause" [patterns, body, locals] -> Match [] MatchHeadPrefix <$> (list patterns >>= mapM patternFromTree) <*> rightSide body locals
        _ -> unsupported "clause" clauseTree

-- | A foreign import or a foreign export. The entity text has the same
-- meaning as the string of the declaration in the source text. An empty
-- entity text gives the Haskell name as the entity, as with GHC.
foreignDeclaration :: Tree -> Either Text ForeignDecl
foreignDeclaration tree =
  case tree of
    TreeNode "ImportF" [convention, safety, TreeString entity, name, signature] -> do
      safety' <- case safety of
        TreeNode "Safe" [] -> Right Safe
        TreeNode "Unsafe" [] -> Right Unsafe
        TreeNode "Interruptible" [] -> Right Interruptible
        _ -> unsupported "foreign call safety" safety
      declaration' ForeignImport convention (Just safety') (importEntity (T.strip entity)) name signature
    TreeNode "ExportF" [convention, TreeString entity, name, signature] ->
      declaration' ForeignExport convention Nothing (exportEntity (T.strip entity)) name signature
    _ -> unsupported "foreign declaration" tree
  where
    declaration' direction convention safety entity name signature = do
      convention' <- case convention of
        TreeNode "CCall" [] -> Right CCall
        TreeNode "CApi" [] -> Right CApi
        TreeNode "StdCall" [] -> Right StdCall
        TreeNode "Prim" [] -> Right CPrim
        TreeNode "JavaScript" [] -> Right JavaScript
        _ -> unsupported "calling convention" convention
      ForeignDecl direction convention' safety entity <$> binderFromTree name <*> typeFromTree signature
    importEntity entity
      | T.null entity = ForeignEntityOmitted
      | entity == "dynamic" = ForeignEntityDynamic
      | entity == "wrapper" = ForeignEntityWrapper
      | entity == "static" = ForeignEntityStatic Nothing
      | Just rest <- T.stripPrefix "static " entity = ForeignEntityStatic (Just rest)
      | entity == "&" = ForeignEntityAddress Nothing
      | Just rest <- T.stripPrefix "&" entity = ForeignEntityAddress (Just rest)
      | otherwise = ForeignEntityNamed entity
    exportEntity entity
      | T.null entity = ForeignEntityOmitted
      | otherwise = ForeignEntityNamed entity

-- | A data constructor. A @ForallC@ gives the binders and the context of
-- the constructor in it.
dataConstructor :: Tree -> Either Text DataConDecl
dataConstructor tree =
  case tree of
    TreeNode "ForallC" [binders, context, inner] -> do
      binders' <- list binders >>= mapM typeVariableBinder
      context' <- list context >>= mapM typeFromTree
      inner' <- dataConstructor inner
      case inner' of
        PrefixCon [] [] name fields -> Right (PrefixCon binders' context' name fields)
        InfixCon [] [] left name right -> Right (InfixCon binders' context' left name right)
        RecordCon [] [] name fields -> Right (RecordCon binders' context' name fields)
        GadtCon [] [] names body -> Right (GadtCon [ForallTelescope ForallInvisible binders' | not (null binders')] context' names body)
        _ -> unsupported "nested constructor quantification" inner
    TreeNode "NormalC" [name, fields] -> PrefixCon [] [] <$> binderFromTree name <*> (list fields >>= mapM strictField)
    TreeNode "RecC" [name, fields] -> RecordCon [] [] <$> binderFromTree name <*> (list fields >>= mapM fieldDecl)
    TreeNode "InfixC" [left, name, right] -> InfixCon [] [] <$> strictField left <*> binderFromTree name <*> strictField right
    TreeNode "GadtC" [names, fields, result] -> do
      names' <- list names >>= mapM binderFromTree
      fields' <- list fields >>= mapM strictField
      GadtCon [] [] names' . GadtPrefixBody [(field, ArrowUnrestricted) | field <- fields'] <$> typeFromTree result
    TreeNode "RecGadtC" [names, fields, result] -> do
      names' <- list names >>= mapM binderFromTree
      fields' <- list fields >>= mapM fieldDecl
      GadtCon [] [] names' . GadtRecordBody fields' <$> typeFromTree result
    _ -> unsupported "constructor" tree
  where
    fieldDecl fieldTree =
      case fieldTree of
        TreeNode "(,,)" [name, bang, content] -> do
          name' <- binderFromTree name
          FieldDecl [] [name'] Nothing <$> strictField (TreeNode "(,)" [bang, content])
        _ -> unsupported "record field" fieldTree

-- | A field type of a constructor, with its strictness and its unpack
-- pragma.
strictField :: Tree -> Either Text BangType
strictField tree =
  case tree of
    TreeNode "(,)" [TreeNode "Bang" [TreeNode unpackedness [], TreeNode strictness []], content] -> do
      pragmas <- case unpackedness of
        "NoSourceUnpackedness" -> Right []
        "SourceUnpack" -> Right [Pragma (PragmaUnpack UnpackPragma) "{-# UNPACK #-}"]
        "SourceNoUnpack" -> Right [Pragma (PragmaUnpack NoUnpackPragma) "{-# NOUNPACK #-}"]
        _ -> unsupported "unpackedness" (TreeNode unpackedness [])
      (strict, lazy) <- case strictness of
        "NoSourceStrictness" -> Right (False, False)
        "SourceStrict" -> Right (True, False)
        "SourceLazy" -> Right (False, True)
        _ -> unsupported "strictness" (TreeNode strictness [])
      BangType [] pragmas strict lazy <$> typeFromTree content
    _ -> unsupported "field type" tree

-- | A @deriving@ clause, with its strategy.
derivingClause :: Tree -> Either Text DerivingClause
derivingClause tree =
  case tree of
    TreeNode "DerivClause" [strategy, classes] -> DerivingClause <$> optional strategyFromTree strategy <*> (Right <$> (list classes >>= mapM typeFromTree))
    _ -> unsupported "deriving clause" tree

-- | The strategy of a @deriving@ clause or of a standalone deriving
-- declaration.
strategyFromTree :: Tree -> Either Text DerivingStrategy
strategyFromTree strategy =
  case strategy of
    TreeNode "StockStrategy" [] -> Right DerivingStock
    TreeNode "AnyclassStrategy" [] -> Right DerivingAnyclass
    TreeNode "NewtypeStrategy" [] -> Right DerivingNewtype
    TreeNode "ViaStrategy" [via] -> DerivingVia <$> typeFromTree via
    _ -> unsupported "deriving strategy" strategy

-- | The right side of a binding or of a case alternative, from its body
-- and its local declarations.
rightSide :: Tree -> Tree -> Either Text (Rhs Expr)
rightSide body locals = do
  locals' <- declarations locals
  let localDecls = if null locals' then Nothing else Just locals'
  case body of
    TreeNode "NormalB" [value] -> (\value' -> UnguardedRhs [] value' localDecls) <$> expression value
    TreeNode "GuardedB" [guarded] -> (\branches -> GuardedRhss [] branches localDecls) <$> (list guarded >>= mapM guardedValue)
    _ -> unsupported "body" body

-- | A guard and its value, from a @(Guard, Exp)@ tree.
guardedValue :: Tree -> Either Text (GuardedRhs Expr)
guardedValue tree =
  case tree of
    TreeNode "(,)" [TreeNode "NormalG" [condition], value] -> (\condition' value' -> GuardedRhs [] [GuardExpr condition'] value') <$> expression condition <*> expression value
    TreeNode "(,)" [TreeNode "PatG" [statements], value] -> GuardedRhs [] <$> (list statements >>= mapM qualifier) <*> expression value
    _ -> unsupported "guard" tree
  where
    qualifier statement =
      case statement of
        TreeNode "BindS" [pattern', value] -> GuardPat <$> patternFromTree pattern' <*> expression value
        TreeNode "LetS" [locals] -> GuardLet <$> declarations locals
        TreeNode "NoBindS" [condition] -> GuardExpr <$> expression condition
        _ -> unsupported "guard statement" statement

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
    -- An equality with its two sides is an infix type, as GHC prints it.
    -- The resolver gives @~@ its built-in meaning.
    (TreeNode "EqualityT" [], [left, right]) -> (\left' right' -> TParen (TInfix left' equality Unpromoted right')) <$> typeFromTree left <*> typeFromTree right
    (TreeNode "EqualityT" [], arguments) -> foldl TApp (TCon equality Unpromoted) <$> mapM typeFromTree arguments
    (TreeNode "ForallVisT" [binders, body], []) -> TForall . ForallTelescope ForallVisible <$> (list binders >>= mapM typeVariableBinder) <*> typeFromTree body
    -- A promoted list. A cons onto a promoted list literal becomes one
    -- literal, as in the source text @'[a, b]@.
    (TreeNode "PromotedNilT" [], []) -> Right (TList Promoted [])
    (TreeNode "PromotedConsT" [], [first, rest]) -> do
      first' <- typeFromTree first
      rest' <- typeFromTree rest
      pure $ case rest' of
        TList Promoted items -> TList Promoted (first' : items)
        _ -> TParen (TInfix first' cons Promoted rest')
    (other, _) -> unsupported "type" other
  where
    equality = mkName Nothing NameVarSym "~"
    cons = mkName Nothing NameConSym ":"
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
    -- Template Haskell gives a chain of @UInfixE@ with no fixity, as the
    -- source text has it. Name resolution gives the fixity to the chain.
    TreeNode "UInfixE" [left, operator, right] -> EInfix <$> unresolvedOperand left <*> operatorName operator <*> unresolvedOperand right
    TreeNode "AppTypeE" [function, argument] -> ETypeApp <$> expression function <*> typeFromTree argument
    TreeNode "UnboxedTupE" [fields] -> ETuple Unboxed <$> (list fields >>= mapM (optional expression))
    TreeNode "LamCasesE" [clauses] -> ELambdaCases <$> (list clauses >>= mapM lambdaCaseAlternative)
    TreeNode "MultiIfE" [branches] -> EMultiWayIf <$> (list branches >>= mapM guardedValue)
    TreeNode "DoE" [qualifier, statements] -> EDo <$> (list statements >>= mapM doStatement) <*> (maybe DoPlain DoQualified <$> optional qualifierName qualifier)
    TreeNode "MDoE" [qualifier, statements] -> EDo <$> (list statements >>= mapM doStatement) <*> (maybe DoMdo DoQualifiedMdo <$> optional qualifierName qualifier)
    TreeNode "CompE" [statements] -> list statements >>= comprehension
    TreeNode "ArithSeqE" [range] -> EArithSeq <$> arithmeticSequence range
    _ -> unsupported "expression" tree
  where
    -- An operand that is not @UInfixE@ is not part of the chain.
    unresolvedOperand operandTree =
      case operandTree of
        TreeNode "UInfixE" _ -> expression operandTree
        _ -> operand operandTree
    qualifierName nameTree =
      case nameTree of
        TreeNode "ModName" [TreeString name] -> Right name
        _ -> unsupported "module name" nameTree
    lambdaCaseAlternative clauseTree =
      case clauseTree of
        TreeNode "Clause" [patterns, body, locals] -> LambdaCaseAlt [] <$> (list patterns >>= mapM patternFromTree) <*> rightSide body locals
        _ -> unsupported "clause" clauseTree
    doStatement statement =
      case statement of
        TreeNode "BindS" [pattern', value] -> DoBind <$> patternFromTree pattern' <*> expression value
        TreeNode "LetS" [locals] -> DoLetDecls <$> declarations locals
        TreeNode "NoBindS" [value] -> DoExpr <$> expression value
        TreeNode "RecS" [statements] -> DoRecStmt <$> (list statements >>= mapM doStatement)
        _ -> unsupported "statement" statement
    -- The last statement of a comprehension is its result. One @ParS@
    -- statement before it gives a parallel comprehension.
    comprehension statements =
      case reverse statements of
        TreeNode "NoBindS" [result] : qualifiers ->
          case reverse qualifiers of
            [TreeNode "ParS" [branches]] -> EListCompParallel <$> expression result <*> (list branches >>= mapM (list >=> mapM comprehensionStatement))
            qualifiers' -> EListComp <$> expression result <*> mapM comprehensionStatement qualifiers'
        _ -> Left "A comprehension in a splice result has no result expression."
    comprehensionStatement statement =
      case statement of
        TreeNode "BindS" [pattern', value] -> CompGen <$> patternFromTree pattern' <*> expression value
        TreeNode "LetS" [locals] -> CompLetDecls <$> declarations locals
        TreeNode "NoBindS" [condition] -> CompGuard <$> expression condition
        _ -> unsupported "comprehension statement" statement
    arithmeticSequence range =
      case range of
        TreeNode "FromR" [from] -> ArithSeqFrom <$> expression from
        TreeNode "FromThenR" [from, next] -> ArithSeqFromThen <$> expression from <*> expression next
        TreeNode "FromToR" [from, to] -> ArithSeqFromTo <$> expression from <*> expression to
        TreeNode "FromThenToR" [from, next, to] -> ArithSeqFromThenTo <$> expression from <*> expression next <*> expression to
        _ -> unsupported "range" range
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
    TreeNode "RationalL" [ratio] -> do
      value <- rationalFromTree ratio
      Right $
        if value < 0
          then EParen (ENegate (fractionalLiteral (negate value)))
          else fractionalLiteral value
    TreeNode "StringL" [TreeString string] -> Right (EString string (T.pack (show (T.unpack string))))
    -- A primitive literal has its sign in the literal, as @-1#@ has.
    TreeNode "IntPrimL" [TreeInteger number] -> Right (EInt number TIntHash (T.pack (show number) <> "#"))
    -- The string of a bytes literal has one character for each byte, as
    -- the string of a primitive string literal has.
    TreeNode "BytesPrimL" [TreeNode "Bytes" [TreeString bytes]] -> Right (EStringHash bytes (T.pack (show (T.unpack bytes)) <> "#"))
    TreeNode "StringPrimL" [elements] -> do
      codes <- list elements >>= mapM byteCode
      let bytes = T.pack (map toEnum codes)
      Right (EStringHash bytes (T.pack (show (T.unpack bytes)) <> "#"))
    TreeNode "WordPrimL" [TreeInteger number]
      | number >= 0 -> Right (EInt number TWordHash (T.pack (show number) <> "##"))
    TreeNode "CharPrimL" [TreeChar char] -> Right (ECharHash char (T.pack (show char) <> "#"))
    _ -> unsupported "literal" tree
  where
    integerLiteral number = EInt number TInteger (T.pack (show number))
    fractionalLiteral value = EFloat value TFractional (rationalText value)

-- | The value of a @Rational@ tree. The tree of a ratio has the
-- constructor @:%@ with the numerator and the denominator.
rationalFromTree :: Tree -> Either Text Rational
rationalFromTree tree =
  case tree of
    TreeNode ":%" [TreeInteger top, TreeInteger bottom]
      | bottom /= 0 -> Right (top % bottom)
    _ -> unsupported "rational number" tree

-- | The source text of a fractional literal with a value that is not
-- negative. A value with a finite decimal expansion gets that exact text.
-- A different value has no exact literal. Its text is then the nearest
-- 'Double', but the syntax keeps the exact value.
rationalText :: Rational -> Text
rationalText value =
  case decimalDigits (denominator value) of
    Just places ->
      let scaled = numerator value * 10 ^ places `div` denominator value
          digits = T.pack (show scaled)
          padded = T.replicate (places + 1 - T.length digits) "0" <> digits
          (whole, fraction) = T.splitAt (T.length padded - places) padded
       in whole <> "." <> (if T.null fraction then "0" else fraction)
    Nothing -> T.pack (show (fromRational value :: Double))
  where
    -- The number of decimal places of 1/d, if that number is finite.
    decimalDigits :: Integer -> Maybe Int
    decimalDigits d =
      let (twos, rest) = factor 2 d
          (fives, rest') = factor 5 rest
       in if rest' == 1 then Just (max twos fives) else Nothing
    factor :: Integer -> Integer -> (Int, Integer)
    factor base number
      | number `mod` base == 0 = let (count, rest) = factor base (number `div` base) in (count + 1, rest)
      | otherwise = (0, number)

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
    TreeNode "LitP" [TreeNode "RationalL" [ratio]]
      | Right value <- rationalFromTree ratio,
        value >= 0 ->
          Right (PLit (LitFloat value TFractional (rationalText value)))
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
            -- The flag @BndrInvis@ of a declaration head gives @\@a@.
            tyVarBinderVisibility = case flag of
              TreeNode "BndrInvis" [] -> TyVarBInvisible
              _ -> TyVarBVisible
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
-- | A byte of a primitive string literal.
byteCode :: Tree -> Either Text Int
byteCode tree =
  case tree of
    TreeInteger number
      | number >= 0 && number < 256 -> Right (fromInteger number)
    _ -> unsupported "byte" tree

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
