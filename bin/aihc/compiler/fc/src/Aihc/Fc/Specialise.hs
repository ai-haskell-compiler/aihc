{-# LANGUAGE OverloadedStrings #-}

-- | Specialisation of local recursive functions on static dictionaries.
--
-- The type checker generalizes a local function without a signature, so
-- a loop in a @where@ clause that uses a class method gets a type
-- parameter and a dictionary parameter:
--
-- > go : ∀a. $Dict$Storable a → ForeignPtr a → [a] → IO ()
-- > go = Λa. λ$d. λp. λxs. ... $d ... go @a $d p' xs' ...
-- >
-- > go @Word8 $fStorableWord8 p xs
--
-- Each recursive call passes the parameters on unchanged, and the one
-- call from outside gives a constant dictionary. The method selection in
-- the body stays a selection from a parameter, which is an unknown call
-- at each iteration. This pass copies such a function once for each
-- distinct static prefix that its calls give:
--
-- > $sgo : ForeignPtr Word8 → [Word8] → IO ()
-- > $sgo = λp. λxs. ... $fStorableWord8 ... $sgo p' xs' ...
-- >
-- > $sgo p xs
--
-- The dictionary is then a known constructor in the body, and the
-- simplifier resolves the selection to the instance method.
--
-- The static prefix of a binding is its leading type parameters and the
-- value parameters that follow them while their type is a dictionary.
-- A binding is specialised when it is alone in its group, has a
-- dictionary parameter, passes its own prefix on in every recursive call
-- (see 'recursiveAliases' for a parameter given to another slot), and is
-- applied to its whole prefix at every call from outside. The
-- prefix that a call gives has to be in scope where the binding is: a
-- type variable or a dictionary that a case between the two binds cannot
-- move to the binding. A dictionary argument that is not a variable is
-- bound once before the copy, so the copy does not build it again on
-- each iteration. At most 'specialisationLimit' distinct prefixes get a
-- copy; a binding with more stays as it is.
module Aihc.Fc.Specialise
  ( SpecialiseReport (..),
    specialiseProgram,
    specialisationLimit,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Simplify (collectSpine, freshenExprFrom, isTrivial, maxLocalUnique, substExpr, substTypeExpr)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (substType, substTypes)
import Aihc.Tc.Types (Unique (..))
import Control.Monad (zipWithM)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Either (lefts, rights)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

-- | What the pass did.
data SpecialiseReport = SpecialiseReport
  { -- | Bindings that got copies in place of the original.
    reportSpecialisedBindings :: !Int,
    -- | Copies made, one for each distinct static prefix.
    reportCopies :: !Int,
    -- | Calls from outside the bindings that now name a copy.
    reportRewrittenCalls :: !Int
  }
  deriving (Eq, Show)

instance Semigroup SpecialiseReport where
  SpecialiseReport a b c <> SpecialiseReport d e f = SpecialiseReport (a + d) (b + e) (c + f)

instance Monoid SpecialiseReport where
  mempty = SpecialiseReport 0 0 0

-- | The most distinct static prefixes a binding gets copies for.
specialisationLimit :: Int
specialisationLimit = 4

type Arg = Either Type Expr

-- | A fresh unique for each new binder, and the report so far.
type SpecM = State (Int, SpecialiseReport)

-- | Specialise every local recursive function of the program.
specialiseProgram :: Program -> (Program, SpecialiseReport)
specialiseProgram program =
  let (decls, (_, report)) = runState (mapM onDecl (programDecls program)) (maxLocalUnique program + 1, mempty)
   in (tidyProgram program {programDecls = decls}, report)
  where
    onDecl decl =
      case decl of
        DeclVal declaration -> do
          body <- specialiseExpr Set.empty (valBody declaration)
          pure (DeclVal declaration {valBody = body})
        _ -> pure decl

-- | Walk an expression with the local names in scope at each point, and
-- specialise each recursive group after its parts.
specialiseExpr :: Set Name -> Expr -> SpecM Expr
specialiseExpr scope expr =
  case expr of
    ExVar {} -> pure expr
    ExLit {} -> pure expr
    ExCoercion {} -> pure expr
    ExApp function argument -> ExApp <$> specialiseExpr scope function <*> specialiseExpr scope argument
    ExTyApp function ty -> (`ExTyApp` ty) <$> specialiseExpr scope function
    ExLam binder body -> ExLam binder <$> specialiseExpr (bind binder scope) body
    ExTyLam binder body -> ExTyLam binder <$> specialiseExpr (bind binder scope) body
    ExLet (Bind binder rhs) body -> do
      rhs' <- specialiseExpr scope rhs
      body' <- specialiseExpr (bind binder scope) body
      pure (ExLet (Bind binder rhs') body')
    ExRec binds body -> do
      let inner = List.foldl' (flip (bind . bindBinder)) scope binds
      binds' <- mapM (\(Bind binder rhs) -> Bind binder <$> specialiseExpr inner rhs) binds
      body' <- specialiseExpr inner body
      case binds' of
        [one] -> specialiseRec scope one body'
        _ -> pure (ExRec binds' body')
    ExCase scrutinee binder resultType alternatives -> do
      scrutinee' <- specialiseExpr scope scrutinee
      let withBinder = foldl' (flip bind) scope binder
          onAlt alternative = do
            let altScope = List.foldl' (flip bind) withBinder (altTypeBinders alternative <> altBinders alternative)
            rhs <- specialiseExpr altScope (altRhs alternative)
            pure alternative {altRhs = rhs}
      ExCase scrutinee' binder resultType <$> mapM onAlt alternatives
    ExCast body coercion -> (`ExCast` coercion) <$> specialiseExpr scope body
    ExForeignCall call types arguments -> ExForeignCall call types <$> mapM (specialiseExpr scope) arguments
  where
    bind binder = Set.insert (binderName binder)

-- | Specialise one binding that is alone in its group, when its calls
-- permit it. The scope is the one at the group, without the binding.
specialiseRec :: Set Name -> Bind -> Expr -> SpecM Expr
specialiseRec scope binding@(Bind binder rhs) body =
  case plan of
    Nothing -> pure (ExRec [binding] body)
    Just (prefix, inner, calls, keys) -> do
      copies <- mapM (makeCopy prefix inner) keys
      let names = Map.fromList [(key, binderName (bindBinder copy)) | (key, _, copy) <- copies]
          body' = rewriteCalls name (prefixLength prefix) (names Map.!) body
          wrap (_, lets, copy) rest = foldr ExLet (ExRec [copy] rest) lets
      state (\(supply, report) -> ((), (supply, report <> SpecialiseReport 1 (length copies) (length calls))))
      pure (foldr wrap body' copies)
  where
    name = binderName binder
    plan = do
      (prefix, inner) <- staticPrefix rhs
      let count = prefixLength prefix
      recursive <- occurrences name count inner
      aliases <- concat <$> mapM (recursiveAliases prefix) recursive
      calls <- occurrences name count body
      if null calls then Nothing else Just ()
      if all (staticKey scope) calls then Just () else Nothing
      if all (\key -> all (\(slot, source) -> key !! slot == key !! source) aliases) calls then Just () else Nothing
      let keys = List.nub calls
      if length keys <= specialisationLimit then Just () else Nothing
      types <- mapM (\key -> instantiate (binderType binder) (lefts key) (length (prefixDictionaries prefix))) keys
      pure (prefix, inner, calls, zip keys types)
    makeCopy prefix inner (key, copyType) = do
      let typeSubst = Map.fromList (zip (map binderName (prefixTypes prefix)) (lefts key))
      copyName <- fresh name {nameText = "$s" <> nameText name}
      bound <- mapM bindDictionary (zip (prefixDictionaries prefix) (rights key))
      let valueSubst = Map.fromList [(binderName dictionary, replacement) | (dictionary, replacement, _) <- bound]
          lets = [Bind (Binder bindName (substTypes typeSubst (binderType dictionary))) argument | (dictionary, ExVar bindName, Just argument) <- bound]
          recursed = rewriteCalls name (prefixLength prefix) (const copyName) inner
      copyBody <- freshen recursed
      let copy = Bind (Binder copyName copyType) (substExpr valueSubst (substTypeExpr typeSubst copyBody))
      pure (key, lets, copy)
    -- A trivial argument goes into the copy as it is. Any other argument
    -- is bound once before the copy, so the copy does not build it again
    -- on each iteration.
    bindDictionary (dictionary, argument)
      | isTrivial argument = pure (dictionary, argument, Nothing)
      | otherwise = do
          bindName <- fresh (binderName dictionary)
          pure (dictionary, ExVar bindName, Just argument)

-- | The leading type parameters of a function and the dictionary
-- parameters that follow them, with the body under them.
data Prefix = Prefix
  { prefixTypes :: [Binder],
    prefixDictionaries :: [Binder]
  }

prefixLength :: Prefix -> Int
prefixLength prefix = length (prefixTypes prefix) + length (prefixDictionaries prefix)

-- | Which parameter a recursive call gives for each slot of the prefix.
-- A type slot has to get its own type variable. A dictionary slot can get
-- any dictionary parameter: the type checker gives a loop one parameter
-- for each constraint it collects, with the same class more than once,
-- and the recursive call then gives the first parameter of a class for
-- every slot of that class. The pairs of a slot and the parameter it gets
-- are the aliases of the call. A copy is right when every call from
-- outside gives the two slots of each alias the same argument.
-- 'Nothing' when a slot gets anything else.
recursiveAliases :: Prefix -> [Arg] -> Maybe [(Int, Int)]
recursiveAliases prefix = zipWithM alias [0 ..]
  where
    types = map binderName (prefixTypes prefix)
    dictionaries = map binderName (prefixDictionaries prefix)
    alias slot argument =
      case argument of
        Left (TyVar variable)
          | slot < length types, types !! slot == variable -> Just (slot, slot)
        Right (ExVar variable)
          | slot >= length types, Just source <- List.elemIndex variable dictionaries -> Just (slot, length types + source)
        _ -> Nothing

-- | Peel the static prefix off a right-hand side. 'Nothing' when there is
-- no dictionary parameter, because a copy at a type alone changes no code.
staticPrefix :: Expr -> Maybe (Prefix, Expr)
staticPrefix rhs =
  let (types, afterTypes) = peelTypes rhs
      (dictionaries, inner) = peelDictionaries afterTypes
   in if null dictionaries then Nothing else Just (Prefix types dictionaries, inner)
  where
    peelTypes expr =
      case expr of
        ExTyLam binder body -> let (more, rest) = peelTypes body in (binder : more, rest)
        _ -> ([], expr)
    peelDictionaries expr =
      case expr of
        ExLam binder body
          | isDictionaryType (binderType binder) ->
              let (more, rest) = peelDictionaries body in (binder : more, rest)
        _ -> ([], expr)

-- | A class dictionary type: an application of a @$Dict$@ constructor.
isDictionaryType :: Type -> Bool
isDictionaryType ty =
  case ty of
    TyApp function _ -> isDictionaryType function
    TyCon tyCon -> "$Dict$" `T.isPrefixOf` nameText tyCon
    _ -> False

-- | The first arguments of every call of a name, or 'Nothing' when some
-- occurrence of the name is not a call with that many arguments.
occurrences :: Name -> Int -> Expr -> Maybe [[Arg]]
occurrences target count = go
  where
    go expr =
      case expr of
        ExVar name
          | name == target -> if count == 0 then Just [[]] else Nothing
          | otherwise -> Just []
        ExLit {} -> Just []
        ExCoercion {} -> Just []
        ExApp {} -> spine expr
        ExTyApp {} -> spine expr
        ExLam _ body -> go body
        ExTyLam _ body -> go body
        ExLet (Bind _ rhs) body -> (<>) <$> go rhs <*> go body
        ExRec binds body -> concat <$> mapM go (map bindRhs binds <> [body])
        ExCase scrutinee _ _ alternatives -> (<>) <$> go scrutinee <*> (concat <$> mapM (go . altRhs) alternatives)
        ExCast body _ -> go body
        ExForeignCall _ _ arguments -> concat <$> mapM go arguments
    spine expr =
      let (function, arguments) = collectSpine expr
          inArguments = concat <$> mapM (either (const (Just [])) go) arguments
       in case function of
            ExVar name
              | name == target ->
                  if length arguments >= count then (take count arguments :) <$> inArguments else Nothing
            _ -> (<>) <$> go function <*> inArguments

-- | Replace every call of a name that gives at least the prefix by a call
-- of the name chosen for the prefix, without the prefix.
rewriteCalls :: Name -> Int -> ([Arg] -> Name) -> Expr -> Expr
rewriteCalls target count choose = go
  where
    go expr =
      case expr of
        ExVar {} -> expr
        ExLit {} -> expr
        ExCoercion {} -> expr
        ExApp {} -> spine expr
        ExTyApp {} -> spine expr
        ExLam binder body -> ExLam binder (go body)
        ExTyLam binder body -> ExTyLam binder (go body)
        ExLet (Bind binder rhs) body -> ExLet (Bind binder (go rhs)) (go body)
        ExRec binds body -> ExRec [Bind binder (go rhs) | Bind binder rhs <- binds] (go body)
        ExCase scrutinee binder resultType alternatives ->
          ExCase (go scrutinee) binder resultType [alternative {altRhs = go (altRhs alternative)} | alternative <- alternatives]
        ExCast body coercion -> ExCast (go body) coercion
        ExForeignCall call types arguments -> ExForeignCall call types (map go arguments)
    spine expr =
      let (function, arguments) = collectSpine expr
          arguments' = map (fmap go) arguments
       in case function of
            ExVar name
              | name == target,
                length arguments >= count ->
                  rebuild (ExVar (choose (take count arguments))) (drop count arguments')
            _ -> rebuild (go function) arguments'
    rebuild = List.foldl' (\function argument -> either (ExTyApp function) (ExApp function) argument)

-- | Whether every name a prefix uses is in scope at the binding. A type
-- constructor or a top-level value always is; a local name has to be in
-- the set. A dictionary argument is an application of dictionaries to
-- types and dictionaries, under casts; any other shape is not static.
staticKey :: Set Name -> [Arg] -> Bool
staticKey scope = all static
  where
    static argument =
      case argument of
        Left ty -> typeStatic ty
        Right expr -> exprStatic expr
    exprStatic expr =
      case expr of
        ExVar name -> nameStatic name
        ExTyApp function ty -> exprStatic function && typeStatic ty
        ExApp function argument -> exprStatic function && exprStatic argument
        ExCast body coercion -> exprStatic body && coercionStatic coercion
        _ -> False
    nameStatic name =
      case nameOrigin name of
        OriginTop {} -> True
        OriginLocal {} -> Set.member name scope
    typeStatic ty = all nameStatic (Set.toList (typeVariables ty))
    coercionStatic coercion =
      case coercion of
        CoVar name -> nameStatic name
        CoRefl ty -> typeStatic ty
        CoSym inner -> coercionStatic inner
        CoTrans left right -> coercionStatic left && coercionStatic right
        CoApp left right -> coercionStatic left && coercionStatic right
        CoFun left right -> coercionStatic left && coercionStatic right
        CoForAll binder body -> typeStatic (binderType binder) && coercionStatic body
        CoNth _ inner -> coercionStatic inner
        CoTyConApp _ inners -> all coercionStatic inners
        CoAxiom _ types -> all typeStatic types

typeVariables :: Type -> Set Name
typeVariables ty =
  case ty of
    TyVar name -> Set.singleton name
    TyCon {} -> Set.empty
    TyLit {} -> Set.empty
    TyApp function argument -> typeVariables function <> typeVariables argument
    TyFun r1 r2 argument result -> Set.unions (map typeVariables [r1, r2, argument, result])
    TyForAll binder body -> Set.delete (binderName binder) (typeVariables body) <> typeVariables (binderType binder)
    TyEq left right -> typeVariables left <> typeVariables right

-- | The type of a copy: the type of the binding at the types of the
-- prefix, without the arrows of its dictionaries. 'Nothing' when the
-- type does not show the quantifiers and arrows of the prefix.
instantiate :: Type -> [Type] -> Int -> Maybe Type
instantiate ty types dictionaries =
  case (ty, types) of
    (TyForAll binder body, argument : rest) -> instantiate (substType (binderName binder) argument body) rest dictionaries
    (_, []) | dictionaries == 0 -> Just ty
    (TyFun _ _ _ result, []) -> instantiate result [] (dictionaries - 1)
    _ -> Nothing

fresh :: Name -> SpecM Name
fresh name = state (\(supply, report) -> (name {nameOrigin = OriginLocal (Unique supply)}, (supply + 1, report)))

freshen :: Expr -> SpecM Expr
freshen expr = state (\(supply, report) -> let (copy, supply') = freshenExprFrom supply expr in (copy, (supply', report)))
