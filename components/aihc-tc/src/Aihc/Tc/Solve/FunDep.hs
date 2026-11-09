-- | Improvement from functional dependencies.
--
-- A dependency @U -> V@ of a class says that the parameters at the
-- positions @U@ determine the ones at @V@. When two class constraints agree
-- on the determining parameters, the solver may therefore equate their
-- determined parameters, which solves meta variables that no single
-- constraint could solve on its own. This is improvement: it produces
-- equalities, never evidence, and the dictionaries themselves are still
-- solved by an instance or a given afterwards.
--
-- Three constraints can improve a wanted: another wanted, a given, and the
-- head of an instance whose determining parameters the wanted matches.
--
-- A dependency need not be declared on the wanted's own class. A wanted
-- entails its superclasses, so a dependency of a superclass improves the
-- wanted through the superclass constraint it entails, which shares the
-- wanted's meta variables. @class MonadParsec e s m => MonadParsecDbg e s m@
-- declares no dependency of its own, yet a wanted @MonadParsecDbg t0 t1 m@
-- is improved by the @m -> e s@ dependency of @MonadParsec@ against a given
-- @MonadParsecDbg e s m@, whose superclass @MonadParsec e s m@ determines
-- @t0@ and @t1@.
module Aihc.Tc.Solve.FunDep
  ( improveFunDeps,
    superClassClosure,
    superClassEqualities,
  )
where

import Aihc.Tc.Constraint (Ct (..))
import Aihc.Tc.Env (ClassInfo (..), FunDep (..), InstanceInfo (..))
import Aihc.Tc.FunDep (atPositions, classDependencyArguments, dependencyDeterminers)
import Aihc.Tc.Match (matchTypes)
import Aihc.Tc.Monad (TcM, freshMetaTvOfKind, getClassInstances, getKinds, lookupClass)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unifyTypes)
import Aihc.Tc.Zonk (zonkPred)
import Control.Monad (foldM, forM, forM_, void, when, zipWithM_)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)

-- | Improve the given dictionary constraints against each other, the givens
-- in scope, and the instances of their classes.
--
-- The result says whether improvement solved a meta variable, which means
-- the constraints are worth another attempt.
improveFunDeps :: [Pred] -> [Ct] -> TcM Bool
improveFunDeps givens constraints = do
  improvable <- concat <$> mapM constraintFunDeps constraints
  if null improvable
    then pure False
    else do
      expandedGivens <- givenEqualityCopies <$> superClassClosure givens
      let siblings = map fst improvable
      before <- mapM (zonkPred . ctPred) constraints
      forM_ improvable (uncurry (improveConstraint expandedGivens siblings 0))
      after <- mapM (zonkPred . ctPred) constraints
      pure (before /= after)

-- | The givens together with the superclasses they entail, transitively.
--
-- A superclass of a given holds wherever the given does, so its functional
-- dependencies improve a wanted just as the given's own do. Evidence lookup
-- projects a superclass out of a given on demand, but improvement compares
-- predicates rather than searching for evidence, so the superclasses have to
-- be present in the list it compares against.
superClassClosure :: [Pred] -> TcM [Pred]
superClassClosure givens = reverse <$> foldM add [] givens
  where
    add seen predicate
      | predicate `elem` seen = pure seen
      | otherwise = do
          parents <- superClassesOf predicate
          foldM add (predicate : seen) parents

-- | The givens, together with a copy of each class given for each given
-- equality that names a type variable, with that variable replaced by the
-- other side.
--
-- The givens @(m ~~ m', MonadReader r m')@ hold the class given at @m'@,
-- and a wanted @MonadReader t0 m@ names @m@. The copy @MonadReader r m@
-- lets the dependency @m -> r@ improve @t0@ to @r@. The copies are for
-- improvement only and carry no evidence.
givenEqualityCopies :: [Pred] -> [Pred]
givenEqualityCopies givens =
  givens
    <> [ copy
       | (variable, replacement) <- rewrites,
         predicate@ClassPred {} <- givens,
         let copy = applySubstPred (Map.singleton (tvUnique variable) replacement) predicate,
         copy /= predicate,
         copy `notElem` givens
       ]
  where
    rewrites =
      concat
        [ [(variable, right) | TcTyVar variable <- [left]] <> [(variable, left) | TcTyVar variable <- [right]]
        | EqPred left right <- givens
        ]

-- | The equality constraints that the class constraints entail through
-- their superclasses, transitively.
--
-- A wanted class constraint holds only if its superclasses hold, so an
-- equality superclass of it must hold too. Such an equality can improve a
-- meta variable that the wanted shares with another constraint. The class
-- @(Key p ~ Wrap p) => Ml p@ is an example: a wanted @Ml t0@ entails @Key
-- t0 ~ Wrap t0@, and a wanted @Key t0 ~ Wrap P@ then gives @t0 := P@.
-- The equalities are for improvement only and carry no evidence.
superClassEqualities :: [Ct] -> TcM [Pred]
superClassEqualities constraints = do
  predicates <- mapM (zonkPred . ctPred) constraints
  closure <- superClassClosure [predicate | predicate@ClassPred {} <- predicates]
  pure [predicate | predicate@EqPred {} <- closure]

-- | The superclass constraints of a class given, instantiated at its
-- arguments. Every other predicate has none.
superClassesOf :: Pred -> TcM [Pred]
superClassesOf predicate =
  case predicate of
    ClassPred className arguments -> do
      classInfo <- lookupClass className
      case classInfo of
        Nothing -> pure []
        Just info -> do
          kinds <- getKinds
          let substitution =
                Map.fromList
                  [(tvUnique tyVar, argument) | (tyVar, argument) <- zip (ciTyVars info) arguments]
          pure (mapMaybe (constraintTypeToPred kinds . applySubst substitution) (ciSuperClassTypes info))
    _ -> pure []

-- | The class constraints a wanted entails whose class declares a
-- functional dependency: the wanted itself and its superclass closure,
-- instantiated at the wanted's arguments. Every other constraint entails
-- none.
--
-- The closure visits each distinct predicate once, so a cyclic or a wide
-- superclass hierarchy terminates after as many steps as it has classes.
constraintFunDeps :: Ct -> TcM [(Pred, ClassInfo)]
constraintFunDeps constraint =
  case ctPred constraint of
    ClassPred {} -> predicateFunDeps =<< zonkPred (ctPred constraint)
    _ -> pure []

-- | The class predicates that a class predicate entails, itself included,
-- whose class declares a functional dependency.
predicateFunDeps :: Pred -> TcM [(Pred, ClassInfo)]
predicateFunDeps predicate =
  case predicate of
    ClassPred {} -> do
      closure <- superClassClosure [predicate]
      catMaybes <$> mapM withFunDeps closure
    _ -> pure []
  where
    withFunDeps entailed =
      case entailed of
        ClassPred className _ -> do
          classInfo <- lookupClass className
          pure $ case classInfo of
            Just info | not (null (ciFunDeps info)) -> Just (entailed, info)
            _ -> Nothing
        _ -> pure Nothing

-- | Improve one entailed constraint from every source in turn. The
-- constraint shares its meta variables with the wanted that entails it, so
-- solving them improves the wanted. A constraint improves against itself
-- trivially, so the sibling list may hold it.
--
-- The depth counts the instance contexts that the improvement went
-- through. See 'improveThroughContext'.
improveConstraint :: [Pred] -> [Pred] -> Int -> Pred -> ClassInfo -> TcM ()
improveConstraint givens siblings depth constraint info = do
  siblingPredicates <- mapM zonkPred siblings
  forM_ (ciFunDeps info) $ \dependency -> do
    forM_ (givens <> siblingPredicates) $ \other ->
      improveFromPredicate info dependency constraint other
    improveFromInstances givens siblings depth info dependency constraint

-- | Improve a wanted from another class constraint of the same class.
improveFromPredicate :: ClassInfo -> FunDep -> Pred -> Pred -> TcM ()
improveFromPredicate info dependency constraint other =
  case other of
    ClassPred otherClass otherArguments
      | tyConKey otherClass == tyConKey (ciTyCon info) -> do
          predicate <- zonkPred constraint
          case predicate of
            ClassPred _ arguments -> do
              arguments' <- classDependencyArguments info arguments
              otherArguments' <- classDependencyArguments info otherArguments
              determinerPositions <- dependencyDeterminers info dependency
              let determiners = atPositions determinerPositions
              -- The determining parameters must be equal now. If only a
              -- later solution of their meta variables made them equal,
              -- the two constraints could still be about different types.
              -- An occurrence whose kind a given equality refined is still
              -- the same variable.
              when (sameTypes (determiners arguments') (determiners otherArguments')) $
                improveEqualities (determined dependency arguments') (determined dependency otherArguments')
            _ -> pure ()
    _ -> pure ()

sameTypes :: [TcType] -> [TcType] -> Bool
sameTypes left right = length left == length right && and (zipWith sameType left right)

-- | Improve a wanted from the head of every instance whose determining
-- parameters it matches.
--
-- The match is one-way: the instance variables stand for the types the
-- wanted names, and a meta variable of the wanted is rigid. Matching it
-- against a concrete instance parameter instead would commit the wanted to
-- an instance that a later solution could contradict.
--
-- An instance accepted under the liberal coverage condition can take a
-- dependent parameter from its context rather than from its head, which
-- leaves a variable of the instance in the parameter the match determined.
-- The head of such an instance says nothing about the wanted, so the
-- improvement goes through the context of the instance instead. See
-- 'improveThroughContext'.
improveFromInstances :: [Pred] -> [Pred] -> Int -> ClassInfo -> FunDep -> Pred -> TcM ()
improveFromInstances givens siblings depth info dependency constraint = do
  instances <- getClassInstances (ciTyCon info)
  predicate <- zonkPred constraint
  case predicate of
    ClassPred _ arguments -> do
      arguments' <- classDependencyArguments info arguments
      determinerPositions <- dependencyDeterminers info dependency
      let determiners = atPositions determinerPositions
      candidates <- fmap catMaybes . forM instances $ \instanceInfo -> do
        instanceHead <- classDependencyArguments info (iiHead instanceInfo)
        pure $ do
          substitution <- matchTypes (determiners instanceHead) (determiners arguments')
          pure (instanceInfo, map (applySubst substitution) (determined dependency instanceHead))
      forM_ candidates $ \(instanceInfo, instanceDetermined) ->
        if any (\tyVar -> any (typeMentionsTyVar tyVar) instanceDetermined) (iiTyVars instanceInfo)
          then case candidates of
            [_] -> improveThroughContext givens siblings depth instanceInfo arguments
            _ -> pure ()
          else improveEqualities instanceDetermined (determined dependency arguments')
    _ -> pure ()

-- | Improve a wanted from the context of the one instance that its
-- determining parameters select, when the head of that instance does not
-- determine the dependent parameters.
--
-- The standard lifting instance of a monad transformer is an example:
-- @instance MonadParsec e s m => MonadParsec e s (ReaderT r m)@. Its head
-- matches every wanted on @ReaderT r m@, and the solver selects it for
-- each one. The wanted @MonadParsec t0 t1 (ReaderT r (ParsecT Void Text
-- Identity))@ then needs @MonadParsec t0 t1 (ParsecT Void Text Identity)@,
-- and the instance for @ParsecT@ improves @t0@ and @t1@. The solver
-- solves an instance context as one unit and does not keep its constraints
-- as wanteds, so the improvement has to go through the context here.
--
-- The whole head has to match the wanted, because only then does the
-- solver select the instance. No other instance may match the determining
-- parameters, because an overlapping instance could be selected instead.
--
-- A variable of the instance that the head does not bind stands for a type
-- that only the context determines. Each such variable becomes a fresh meta
-- variable for the improvement, as it would for the wanteds of the context,
-- so that one constraint of the context can improve another. The instance
-- @(AllNullary a l, AllNullary b r, And l r all) => AllNullary (a :+: b)
-- all@ of @aeson@ is an example: @l@ and @r@ come from the first two
-- constraints, and @all@ from the third. The constraints improve each
-- other until a pass changes none of them.
--
-- An instance context can be as large as the head or larger under
-- @UndecidableInstances@, so a depth limit stops a chain of contexts that
-- does not end.
improveThroughContext :: [Pred] -> [Pred] -> Int -> InstanceInfo -> [TcType] -> TcM ()
improveThroughContext givens siblings depth instanceInfo arguments
  | depth >= contextDepthLimit = pure ()
  | otherwise =
      case matchTypes (iiHead instanceInfo) arguments of
        Nothing -> pure ()
        Just substitution -> do
          let bound tyVar = Map.member (tvUnique tyVar) substitution
              unbound = filter (not . bound) (iiTyVars instanceInfo)
          fresh <-
            forM unbound $ \tyVar -> do
              meta <- freshMetaTvOfKind (applySubst substitution (tvKind tyVar))
              pure (tvUnique tyVar, meta)
          let instantiation = substitution <> Map.fromList fresh
              context = [applySubstPred instantiation contextPredicate | contextPredicate <- iiContext instanceInfo]
              improveContext :: Int -> TcM ()
              improveContext passes = do
                before <- mapM zonkPred context
                improvable <- concat <$> mapM predicateFunDeps context
                forM_ improvable (uncurry (improveConstraint givens (siblings <> context) (depth + 1)))
                after <- mapM zonkPred context
                when (before /= after && passes > 1) (improveContext (passes - 1))
          improveContext contextPassLimit

-- | The most passes over an instance context that one improvement makes.
contextPassLimit :: Int
contextPassLimit = 8

-- | The maximum number of instance contexts that one improvement goes
-- through.
contextDepthLimit :: Int
contextDepthLimit = 32

determined :: FunDep -> [TcType] -> [TcType]
determined dependency = atPositions (fdDetermined dependency)

-- | Equate the determined parameters. Improvement carries no evidence, so
-- the equality is solved by unification alone. Types that do not unify are
-- left to the constraint itself to report as unsolved.
improveEqualities :: [TcType] -> [TcType] -> TcM ()
improveEqualities left right
  | length left /= length right = pure ()
  | otherwise = zipWithM_ improveOne left right
  where
    improveOne leftType rightType
      | leftType == rightType = pure ()
      | otherwise = void (unifyTypes leftType rightType)
