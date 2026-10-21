{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Dictionary specialization for local loops and top-level functions.
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
    specialiseTopProgram,
    specialisationLimit,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Simplify (collectSpine, freshenExprFrom, isDictionaryType, isTrivial, maxLocalUnique, substExpr, substTypeExpr)
import Aihc.Fc.Size (exprSize, programSize)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (substType, substTypes, typeEnvFromProgram)
import Aihc.Resolve (PackageId)
import Aihc.Tc.Types (Unique (..))
import Control.Monad (zipWithM)
import Control.Monad.Trans.State.Strict (State, execState, get, modify', put, runState, state)
import Data.Either (lefts, rights)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

-- | What the pass did.
data SpecialiseReport = SpecialiseReport
  { -- | Bindings that got specialized copies.
    reportSpecialisedBindings :: !Int,
    -- | Copies made, one for each distinct static prefix.
    reportCopies :: !Int,
    -- | Calls that now name a copy.
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

-- | Specialize closed dictionary calls to top-level functions. Keep the
-- generic declarations for calls that do not match a selected prefix.
-- Each function gets at most four copies. Each copy costs at most 256
-- nodes. The total allowance is one quarter of the input size, or 32
-- nodes for a small program. Three rounds expose calls in new copies.
-- The pipeline runs this pass once, after the main optimizer.
specialiseTopProgram :: PackageId -> Program -> (Program, SpecialiseReport)
specialiseTopProgram primPackage program =
  let initial = TopState (maxLocalUnique program + 1) names Map.empty Map.empty allowance mempty
      (decls, final) = runState (rounds 3 (programDecls program)) initial
   in (tidyProgram program {programDecls = decls}, topReport final)
  where
    names = Set.fromList [valName declaration | DeclVal declaration <- programDecls program]
    allowance = max 32 (programSize primPackage program `div` 4)
    env = typeEnvFromProgram primPackage program
    -- Dictionary producers remain shared inputs. Their specialization would
    -- change the keys of calls that consume those dictionaries.
    candidates = Map.fromList [(valName declaration, (declaration, prefix, inner)) | DeclVal declaration <- programDecls program, Just (prefix, inner) <- [staticPrefix (valBody declaration)], Just result <- [instantiate (valType declaration) (map (TyVar . binderName) (prefixTypes prefix)) (length (prefixDictionaries prefix))], not (isDictionaryType result)]
    rounds :: Int -> [Decl] -> State TopState [Decl]
    rounds 0 decls = pure decls
    rounds remaining decls = do
      current <- get
      let calls = execState (mapM_ (onValue (walkCalls collect)) decls) Map.empty
          requests = List.sortOn (\(key, count) -> (negate count, key)) (Map.toList calls)
          unseen = filter (\(key, _) -> Map.notMember key (topCopies current)) requests
      copies <- concat <$> mapM (makeCopy . fst) unseen
      if null copies
        then pure decls
        else do
          rewritten <- mapM (onValue (walkCalls rewrite)) (decls <> copies)
          rounds (remaining - 1) rewritten
    collect :: Map.Map Name Expr -> Expr -> [Arg] -> State (Map.Map (Name, [Arg]) Int) Expr
    collect aliases function arguments = do
      case function of
        ExVar name
          | Just (_, prefix, _) <- Map.lookup name candidates,
            let count = prefixLength prefix,
            length arguments >= count,
            let key = staticArguments prefix (map (fmap (substExpr aliases)) (take count arguments)),
            staticKey Set.empty key ->
              modify' (Map.insertWith (+) (name, key) (1 :: Int))
        _ -> pure ()
      pure (rebuildCall function arguments)
    makeCopy :: (Name, [Arg]) -> State TopState [Decl]
    makeCopy key@(name, arguments) = do
      current <- get
      case Map.lookup name candidates of
        Just (declaration, prefix, inner)
          | Map.findWithDefault 0 name (topCounts current) < specialisationLimit,
            Just copyType <- instantiateStatic prefix (valType declaration) (lefts arguments) -> do
              let static = staticTypes prefix
                  types = Map.fromList (zip (map binderName static) (lefts arguments))
                  -- A let preserves lazy dictionary evaluation and prevents
                  -- substitution from duplicating an argument inside a copy.
                  bound = zip (prefixDictionaries prefix) (rights arguments)
                  substitutions = Map.fromList [(binderName binder, argument) | (binder, argument) <- bound, isTrivial argument]
                  lets = [(binder, argument) | (binder, argument) <- bound, not (isTrivial argument)]
                  -- The type parameters that no dictionary names stay
                  -- parameters of the copy.
                  kept = [binder {binderType = substTypes types (binderType binder)} | binder <- prefixTypes prefix, binderName binder `notElem` map binderName static]
                  body = foldr ExTyLam (substTypeExpr types (foldr (\(binder, argument) rest -> ExLet (Bind binder argument) rest) (substExpr substitutions inner) lets)) kept
                  cost = 1 + exprSize env body
              if cost > 256 || cost > topBudget current
                then pure []
                else do
                  let copyName = freshTopName (topNames current) name 0
                      (body', supply) = freshenExprFrom (topSupply current) body
                      first = Map.notMember name (topCounts current)
                  put
                    current
                      { topSupply = supply,
                        topNames = Set.insert copyName (topNames current),
                        topCopies = Map.insert key copyName (topCopies current),
                        topCounts = Map.insertWith (+) name 1 (topCounts current),
                        topBudget = topBudget current - cost,
                        topReport = topReport current <> SpecialiseReport (if first then 1 else 0) 1 0
                      }
                  pure [DeclVal declaration {valVis = Private, valName = copyName, valType = copyType, valBody = body'}]
        _ -> pure []
    rewrite :: Map.Map Name Expr -> Expr -> [Arg] -> State TopState Expr
    rewrite aliases function arguments = do
      current <- get
      case function of
        ExVar name
          | Just (_, prefix, _) <- Map.lookup name candidates,
            let count = prefixLength prefix,
            length arguments >= count,
            Just copy <- Map.lookup (name, staticArguments prefix (map (fmap (substExpr aliases)) (take count arguments))) (topCopies current) -> do
              modify' (\st -> st {topReport = topReport st <> SpecialiseReport 0 0 1})
              pure (rebuildCall (ExVar copy) (keptArguments prefix (take count arguments) <> drop count arguments))
        _ -> pure (rebuildCall function arguments)

-- | The type parameters of a prefix that a dictionary parameter names. A
-- top-level copy is made for these types and the dictionaries. The other
-- type parameters stay parameters of the copy, so that the calls of a
-- function such as @pBind@ at many result types share one copy for each
-- dictionary.
staticTypes :: Prefix -> [Binder]
staticTypes prefix =
  [binder | binder <- prefixTypes prefix, Set.member (binderName binder) named]
  where
    named = Set.unions (map (typeVariables . binderType) (prefixDictionaries prefix))

-- | The arguments of a call for the static types and the dictionaries of a
-- prefix: the key of a top-level copy.
staticArguments :: Prefix -> [Arg] -> [Arg]
staticArguments prefix arguments =
  [argument | (binder, argument) <- zip (prefixTypes prefix) arguments, isStatic binder]
    <> drop (length (prefixTypes prefix)) arguments
  where
    static = map binderName (staticTypes prefix)
    isStatic binder = binderName binder `elem` static

-- | The type arguments of a call for the type parameters that a top-level
-- copy keeps.
keptArguments :: Prefix -> [Arg] -> [Arg]
keptArguments prefix arguments =
  [argument | (binder, argument) <- zip (prefixTypes prefix) arguments, binderName binder `notElem` static]
  where
    static = map binderName (staticTypes prefix)

-- | The type of a top-level copy: the type of the function at the static
-- types of the prefix, with the other quantifiers kept and without the
-- arrows of the dictionaries. The quantifiers of the type and the type
-- lambdas of the body are in the same order.
instantiateStatic :: Prefix -> Type -> [Type] -> Maybe Type
instantiateStatic prefix ty0 types0 = go ty0 (prefixTypes prefix) types0
  where
    static = map binderName (staticTypes prefix)
    go ty binders types =
      case (ty, binders) of
        (TyForAll binder body, parameter : rest)
          | binderName parameter `elem` static,
            argument : more <- types ->
              go (substType (binderName binder) argument body) rest more
          | binderName parameter `elem` static -> Nothing
          | otherwise -> TyForAll binder <$> go body rest types
        (_, []) | null types -> instantiate ty [] (length (prefixDictionaries prefix))
        _ -> Nothing

-- | State shared by all rounds, so each limit applies to the whole pass.
data TopState = TopState
  { topSupply :: Int,
    topNames :: Set Name,
    topCopies :: Map.Map (Name, [Arg]) Name,
    topCounts :: Map.Map Name Int,
    topBudget :: Int,
    topReport :: SpecialiseReport
  }

-- | Select a name that no value declaration uses.
freshTopName :: Set Name -> Name -> Int -> Name
freshTopName used original index =
  let candidate = original {nameText = "$s" <> nameText original <> "_" <> T.pack (show index)}
   in if Set.member candidate used
        then freshTopName used original (index + 1)
        else candidate

onValue :: (Monad m) => (Expr -> m Expr) -> Decl -> m Decl
onValue action (DeclVal declaration) = do
  body <- action (valBody declaration)
  pure (DeclVal declaration {valBody = body})
onValue _ declaration = pure declaration

-- | Visit complete call spines once. Track closed dictionary aliases so
-- recursive calls can reuse a copy with a compound dictionary argument.
walkCalls :: (Monad m) => (Map.Map Name Expr -> Expr -> [Arg] -> m Expr) -> Expr -> m Expr
walkCalls action = go Map.empty
  where
    go aliases expr = case expr of
      ExVar {} -> pure expr
      ExLit {} -> pure expr
      ExCoercion {} -> pure expr
      ExApp {} -> spine aliases expr
      ExTyApp {} -> spine aliases expr
      ExLam binder body -> ExLam binder <$> go (forget [binder] aliases) body
      ExTyLam binder body -> ExTyLam binder <$> go (forget [binder] aliases) body
      ExLet (Bind binder rhs) body -> do
        rhs' <- go aliases rhs
        let argument = substExpr aliases rhs'
            inner = forget [binder] aliases
            next
              | isDictionaryType (binderType binder), staticKey Set.empty [Right argument] = Map.insert (binderName binder) argument inner
              | otherwise = inner
        ExLet (Bind binder rhs') <$> go next body
      ExRec binds body -> do
        let inner = forget (map bindBinder binds) aliases
        ExRec <$> mapM (\(Bind binder rhs) -> Bind binder <$> go inner rhs) binds <*> go inner body
      ExCase scrutinee binder alternatives -> do
        let inner = forget (maybe [] pure binder) aliases
            onAlt alternative = do
              rhs <- go (forget (altTypeBinders alternative <> altBinders alternative) inner) (altRhs alternative)
              pure alternative {altRhs = rhs}
        ExCase <$> go aliases scrutinee <*> pure binder <*> traverse onAlt alternatives
      ExAbsurd scrutinee resultType -> (`ExAbsurd` resultType) <$> go aliases scrutinee
      ExCast body coercion -> (`ExCast` coercion) <$> go aliases body
      ExForeignCall call types arguments -> ExForeignCall call types <$> mapM (go aliases) arguments
    spine aliases expr = do
      let (function, arguments) = collectSpine expr
      function' <- go aliases function
      arguments' <- mapM (traverse (go aliases)) arguments
      action aliases function' arguments'
    forget binders aliases = foldr (Map.delete . binderName) aliases binders

rebuildCall :: Expr -> [Arg] -> Expr
rebuildCall = List.foldl' (\function argument -> either (ExTyApp function) (ExApp function) argument)

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
    ExCase scrutinee binder alternatives -> do
      scrutinee' <- specialiseExpr scope scrutinee
      let withBinder = foldl' (flip bind) scope binder
          onAlt alternative = do
            let altScope = List.foldl' (flip bind) withBinder (altTypeBinders alternative <> altBinders alternative)
            rhs <- specialiseExpr altScope (altRhs alternative)
            pure alternative {altRhs = rhs}
      ExCase scrutinee' binder <$> mapM onAlt alternatives
    ExAbsurd scrutinee resultType -> (`ExAbsurd` resultType) <$> specialiseExpr scope scrutinee
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
      copyName <- fresh name {nameText = derivedLocalText "$s" (nameText name)}
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
        ExCase scrutinee _ (NE.toList -> alternatives) -> (<>) <$> go scrutinee <*> (concat <$> mapM (go . altRhs) alternatives)
        ExAbsurd scrutinee _ -> go scrutinee
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
        ExCase scrutinee binder alternatives ->
          ExCase (go scrutinee) binder (fmap (\alternative -> alternative {altRhs = go (altRhs alternative)}) alternatives)
        ExAbsurd scrutinee resultType -> ExAbsurd (go scrutinee) resultType
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
