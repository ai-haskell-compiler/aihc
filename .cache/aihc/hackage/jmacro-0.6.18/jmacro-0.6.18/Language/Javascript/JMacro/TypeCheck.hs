{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, PatternGuards, RankNTypes, FlexibleContexts #-}

module Language.Javascript.JMacro.TypeCheck where

import Language.Javascript.JMacro.Base
import Language.Javascript.JMacro.Types

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.Either
import Data.Map (Map)
import Data.Maybe(catMaybes)
import Data.List(intercalate, nub, transpose)
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Data.Set(Set)
import qualified Data.Set as S

import Text.PrettyPrint.Leijen.Text hiding ((<$>))


-- Utility

eitherIsLeft :: Either a b -> Bool
eitherIsLeft (Left _) = True
eitherIsLeft _ = False

partitionOut :: (a -> Maybe b) -> [a] -> ([b],[a])
partitionOut f xs' = foldr go ([],[]) xs'
    where go x ~(bs,as)
             | Just b <- f x = (b:bs,as)
             | otherwise = (bs,x:as)

zipWithOrChange :: (a -> a -> b) -> (a -> b) -> [a] -> [a] -> [b]
zipWithOrChange f g xss yss = go xss yss
    where go (x:xs) (y:ys) = f x y : go xs ys
          go xs@(_:_) _ = map g xs
          go _ ys = map g ys

zipWithOrIdM :: Monad m => (a -> a -> m a) -> [a] -> [a] -> m [a]
zipWithOrIdM f xs ys = sequence $ zipWithOrChange f return xs ys

unionWithM :: (Monad m, Ord key) => (val -> val -> m val) -> Map key val -> Map key val -> m (Map key val)
unionWithM f m1 m2 = T.sequence $ M.unionWith (\xm ym -> join $ liftM2 f xm ym) (M.map return m1) (M.map return m2)

intersectionWithM :: (Monad m, Ord key) => (val -> val -> m b) -> Map key val -> Map key val -> m (Map key b)
intersectionWithM f m1 m2 = T.sequence $ M.intersectionWith f m1 m2

class Compos1 t where
    compos1 :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
           -> (t -> m t) -> t -> m t

instance Compos1 JType where
    compos1 ret app f v =
        case v of
          JTFunc args body -> ret JTFunc `app` mapM' f args `app` f body
          JTForall vars t -> ret JTForall `app` ret vars `app` f t
          JTList t -> ret JTList `app` f t
          JTMap t -> ret JTMap `app` f t
          JTRecord t m -> ret JTRecord `app` f t `app` m'
              where (ls,ts) = unzip $ M.toList m
                    m' = ret (M.fromAscList . zip ls) `app` mapM' f ts
          x -> ret x
      where
        mapM' g = foldr (app . app (ret (:)) . g) (ret [])

composOp1 :: Compos1 t => (t -> t) -> t -> t
composOp1 f = runIdentity . composOpM1 (Identity . f)
composOpM1 :: (Compos1 t, Monad m) => (t -> m t) -> t -> m t
composOpM1 = compos1 return ap
composOpM1_ :: (Compos1 t, Monad m) => (t -> m ()) -> t -> m ()
composOpM1_ = composOpFold1 (return ()) (>>)
composOpFold1 :: Compos1 t => b -> (b -> b -> b) -> (t -> b) -> t -> b
composOpFold1 z c f = unC . compos1 (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)
newtype C b a = C { unC :: b }

-- Basic Types and TMonad
data StoreVal = SVType JType
              | SVConstrained (Set Constraint)
                deriving Show
              {- -- | SVFreshType Int -}

data TCState = TCS {tc_env :: [Map Ident JType],
                    tc_vars :: Map Int StoreVal,
                    tc_stack :: [Set Int],
                    tc_frozen :: Set Int,
                    tc_varCt :: Int,
                    tc_context :: [TMonad String]}

instance Show TCState where
    show (TCS env vars stack frozen varCt cxt) =
        "env: " ++ show env ++ "\n" ++
        "vars: " ++ show vars ++ "\n" ++
        "stack: " ++ show stack ++ "\n" ++
        "frozen: " ++ show frozen ++ "\n" ++
        "varCt: " ++ show varCt

tcStateEmpty :: TCState
tcStateEmpty = TCS [M.empty] M.empty [S.empty] S.empty 0 []

newtype TMonad a = TMonad (ExceptT String (State TCState) a) deriving (Functor, Monad, MonadState TCState, MonadError String)

instance Applicative TMonad where
    pure = return
    (<*>) = ap

class JTypeCheck a where
    typecheck :: a -> TMonad JType

evalTMonad :: TMonad a -> Either String a
evalTMonad (TMonad x) = evalState (runExceptT x) tcStateEmpty

runTMonad :: TMonad a -> (Either String a, TCState)
runTMonad (TMonad x) = runState (runExceptT x) tcStateEmpty

withContext :: TMonad a -> TMonad String -> TMonad a
withContext act cxt = do
  cs <- tc_context <$> get
  modify (\s -> s {tc_context = cxt : cs})
  res <- act
  modify (\s -> s {tc_context = cs})
  return res

traversem_ :: (F.Foldable t, Monad f) => (a -> f b) -> t a -> f ()
traversem_ f = F.foldr ((>>) . f) (return ())

--assums x is resolved
freeVarsWithNames :: JType -> TMonad (Map Int String)
freeVarsWithNames x =
  intsToNames . (\(a,_,_) -> a) <$>
       execStateT (go x) (M.empty, S.empty, 0)
    where
      go :: JType -> StateT (Map Int (Either String Int), Set String, Int) TMonad ()
      go (JTFree vr) = handleVR vr
      go (JTRigid vr cs) = handleVR vr >> traversem_ (go . fromC) cs
      go v = composOpM1_ go v

      handleVR (mbName, ref) = do
        (m,ns,ct) <- get
        case M.lookup ref m of
          Just (Left _) -> return ()
          Just (Right _) -> case mbName of
                              Just name -> putName name ref
                              Nothing -> return ()
          Nothing -> do
            case mbName of
              Just name -> putName name ref
              Nothing -> put (M.insert ref (Right ct) m, ns, ct + 1)
            mapM_ (go . fromC) =<< lift (lookupConstraintsList (mbName, ref))

      putName n ref = do
        (m,ns,ct) <- get
        let n' = mkUnique ns n 0
        put (M.insert ref (Left n') m, S.insert n' ns, ct)

      mkUnique :: Set String -> String -> Int -> String
      mkUnique ns n i
          | n' `S.member` ns = mkUnique ns n (i + 1)
          | otherwise = n'
          where n' | i == 0 = n
                   | otherwise = n ++ show i

      fromC (Sub t) = t
      fromC (Super t) = t

      intsToNames x = fmap (either id go) x
          where go i = mkUnique (S.fromList $ lefts $ M.elems x) (int2Name i) 0
                int2Name i | q == 0 = [letter]
                           | otherwise = letter : show q
                    where (q,r) = divMod i 26
                          letter = toEnum (fromEnum 'a' + r)


prettyType :: JType -> TMonad String
prettyType x = do
  xt <- resolveType x
  names <- freeVarsWithNames xt
  let replaceNames (JTFree ref) = JTFree $ fixRef ref
      replaceNames (JTForall refs t) = JTForall (map fixRef refs) $ replaceNames t
      replaceNames v = composOp1 replaceNames v

      fixRef (_,ref) = (M.lookup ref names, ref)

      prettyConstraints ref = map go <$> lookupConstraintsList (Nothing, ref)
          where
            myName = case M.lookup ref names of
                       Just n -> n
                       Nothing -> "t_"++show ref
            go (Sub t) = myName ++ " <: " ++ (show $ jsToDoc $ replaceNames t)
            go (Super t) = (show $ jsToDoc $ replaceNames t) ++ " <: " ++ myName

  constraintStrings <- nub . concat <$> mapM prettyConstraints (M.keys names)

  let constraintStr
          | null constraintStrings = ""
          | otherwise = "(" ++ intercalate ", " constraintStrings ++ ") => "

  return $ constraintStr ++ (show . jsToDoc $ replaceNames xt)

tyErr0 :: String -> TMonad a
tyErr0 x = throwError x

tyErr1 :: String -> JType -> TMonad b
tyErr1 s t = do
  st <- prettyType t
  throwError $ s ++ ": " ++ st

tyErr2ext :: String -> String -> String -> JType -> JType -> TMonad a
tyErr2ext s s1 s2 t t' = do
  st <- prettyType t
  st' <- prettyType t'
  throwError $ s ++ ". " ++ s1 ++ ": " ++ st ++ ", " ++ s2 ++ ": " ++ st'

tyErr2Sub :: JType -> JType -> TMonad a
tyErr2Sub t t' = tyErr2ext "Couldn't apply subtype relation" "Subtype" "Supertype" t t'

prettyEnv :: TMonad [Map Ident String]
prettyEnv = mapM (T.mapM prettyType) . tc_env =<< get

runTypecheckRaw :: JTypeCheck a => a -> (Either String JType, TCState)
runTypecheckRaw x = runTMonad (typecheckMain x)

runTypecheckFull :: JTypeCheck a => a -> (Either String (String, [Map Ident String]), TCState)
runTypecheckFull x = runTMonad $ do
                       r <- prettyType =<< typecheckMain x
                       e <- prettyEnv
                       return (r,e)

runTypecheck :: JTypeCheck a => a -> Either String String
runTypecheck x = evalTMonad $ prettyType =<< typecheckMain x

evalTypecheck :: JTypeCheck a => a -> Either String [Map Ident String]
evalTypecheck x = evalTMonad $ do
                    _ <- typecheckMain x
                    e <- prettyEnv
                    return e

typecheckMain :: JTypeCheck a => a -> TMonad JType
typecheckMain x = go `catchError` handler
    where go = do
            r <- typecheck x
            setFrozen . S.unions . tc_stack =<< get
            tryCloseFrozenVars
            return r
          handler e = do
                 cxt <- tc_context <$> get
                 throwError =<< (unlines . (e:) <$> sequence cxt)

-- Manipulating VarRefs and Constraints

addToStack :: Ord a => a -> [Set a] -> [Set a]
addToStack v (s:ss) = S.insert v s : ss
addToStack _ _ = error "addToStack: no sets" --[S.singleton v]

newVarRef :: TMonad VarRef
newVarRef = do
  v <- tc_varCt <$> get
  modify (\s -> s {tc_varCt = v + 1,
                   tc_stack = addToStack v (tc_stack s)})
  return $ (Nothing, v)

newTyVar :: TMonad JType
newTyVar = JTFree <$> newVarRef

mapConstraint :: (Monad m, Functor m) => (JType -> m JType) -> Constraint -> m Constraint
mapConstraint f (Sub t) = Sub <$> f t
mapConstraint f (Super t) = Super <$> f t

partitionCs :: [Constraint] -> ([JType],[JType])
partitionCs [] = ([],[])
partitionCs (Sub t:cs) = (t:subs,sups)
    where (subs,sups) = partitionCs cs
partitionCs (Super t:cs) = (subs,t:sups)
    where (subs,sups) = partitionCs cs


--add mutation
lookupConstraintsList :: VarRef -> TMonad [Constraint]
lookupConstraintsList vr@(_,ref) = do
    vars <- tc_vars <$> get
    case M.lookup ref vars of
      (Just (SVConstrained cs)) -> filter notLoop . nub <$> mapM (mapConstraint resolveType) (S.toList cs)
      (Just (SVType t)) -> tyErr1 "lookupConstraints on instantiated type" t
      Nothing -> return []
  where
    notLoop (Super (JTFree (_,ref'))) | ref == ref' = False
    notLoop (Sub   (JTFree (_,ref'))) | ref == ref' = False
    notLoop _ = True

-- if we instantiate a var to itself, then there's a good chance this results from a looping constraint -- we should be helpful and get rid of any such constraints.
instantiateVarRef :: VarRef -> JType -> TMonad ()
instantiateVarRef vr@(_,ref) (JTFree (_,ref')) | ref == ref' = do
    cs <- lookupConstraintsList vr
    let cs' = simplify cs
    modify (\s -> s {tc_vars = M.insert ref (SVConstrained (S.fromList cs')) (tc_vars s)})
  where simplify (Sub   (JTFree (_,r)):cs) | r == ref = cs
        simplify (Super (JTFree (_,r)):cs) | r == ref = cs
        simplify (c:cs) = c : simplify cs
        simplify x = x

instantiateVarRef vr@(_,ref) t = do
  occursCheck ref t
  cs <- lookupConstraintsList vr
  modify (\s -> s {tc_vars = M.insert ref (SVType t) (tc_vars s)})
  checkConstraints t cs

occursCheck :: Int -> JType -> TMonad ()
occursCheck ref (JTFree (_,i))
  | i == ref = tyErr1 "Occurs check: cannot construct infinite type" (JTFree (Nothing,i))
occursCheck ref x = composOpM1_ (occursCheck ref) x

checkConstraints :: JType -> [Constraint] -> TMonad ()
checkConstraints t cs = mapM_ go cs
    where go (Sub t2) = t <: t2
          go (Super t2) = t2 <: t

addConstraint :: VarRef -> Constraint -> TMonad ()
addConstraint vr@(_,ref) c = case c of
       Sub t -> case t of
                  JTFree _ -> addC c

                  JTForall vars t -> normalizeConstraints . (c : ) =<< lookupConstraintsList vr

                  JTFunc args res -> do
                         mapM_ (occursCheck ref) (res:args)
                         normalizeConstraints . (c :) =<< lookupConstraintsList vr

                  JTRecord t m -> occursCheck ref t >>
                                  F.mapM_ (occursCheck ref) m >>
                                  addRecConstraint (Left (m,t))

                  JTList t' -> do
                         vr' <- newVarRef
                         addConstraint vr' (Sub t')
                         instantiateVarRef vr (JTList (JTFree vr'))

                  JTMap t' -> do
                         vr' <- newVarRef
                         addConstraint vr' (Sub t')
                         instantiateVarRef vr (JTMap (JTFree vr'))

                  JTRigid _ cs -> do
                         (subs,sups) <- partitionCs <$> lookupConstraintsList vr
                         let (subs1,sups1) = partitionCs $ S.toList cs
                         when ((null sups1 && (not . null) sups) ||
                               (null subs1 && (not . null) subs)) $ tyErr2Sub (JTFree vr) t
                         mapM_ (uncurry (<:)) [(x,y) | x <- subs, y <- subs1]
                         mapM_ (uncurry (<:)) [(y,x) | x <- sups, y <- sups1]

                         modify (\s -> s {tc_vars = M.insert ref (SVType t) (tc_vars s)}) --can all this be subsumed by a call to instantiate varref and casing on jtrigid carefully in the <: relationship?
                         -- a polymorphic var is a subtype of another if it is "bigger" on the lattice -- its subtypes are lower and supertypes are higher. sups a > sups b, subs a < subs b
                  _ -> instantiateVarRef vr t

       Super t -> case t of
                  JTFree _ -> addC c

                  JTForall vars t -> normalizeConstraints . (c : ) =<< lookupConstraintsList vr

                  JTFunc args res -> do
                         mapM_ (occursCheck ref) (res:args)
                         normalizeConstraints . (c :) =<< lookupConstraintsList vr

                  JTRecord t m -> occursCheck ref t >>
                                  F.mapM_ (occursCheck ref) m >>
                                  addRecConstraint (Right (m,t))

                  JTList t' -> do
                         vr' <- newVarRef
                         addConstraint vr' (Super t')
                         instantiateVarRef vr (JTList (JTFree vr'))

                  JTMap t' -> do
                         vr' <- newVarRef
                         addConstraint vr' (Super t')
                         instantiateVarRef vr (JTMap (JTFree vr'))

                  JTRigid _ cs -> do
                         (subs,sups) <- partitionCs <$> lookupConstraintsList vr
                         let (subs1,sups1) = partitionCs $ S.toList cs
                         when ((null sups1 && (not . null) sups) ||
                               (null subs1 && (not . null) subs)) $ tyErr2Sub (JTFree vr) t
                         mapM_ (uncurry (<:)) [(y,x) | x <- subs, y <- subs1]
                         mapM_ (uncurry (<:)) [(x,y) | x <- sups, y <- sups1]

                         modify (\s -> s {tc_vars = M.insert ref (SVType t) (tc_vars s)}) --can all this be subsumed by a call to instantiate varref and casing on jtrigid carefully in the <: relationship?
                         -- a polymorphic var is a subtype of another if it is "bigger" on the lattice -- its subtypes are lower and supertypes are higher. sups a > sups b, subs a < subs b

                  _ -> instantiateVarRef vr t
    where
      putCs cs =
        modify (\s -> s {tc_vars = M.insert ref (SVConstrained $ S.fromList $ cs) (tc_vars s)})

      addC constraint = do
        cs <- lookupConstraintsList vr
        modify (\s -> s {tc_vars = M.insert ref (SVConstrained (S.fromList $ constraint:cs)) (tc_vars s)})

      findRecordSubs cs = partitionOut go cs
          where go (Sub (JTRecord t m)) = Just (m,t)
                go _ = Nothing

      findRecordSups cs = partitionOut go cs
          where go (Super (JTRecord t m)) = Just (m,t)
                go _ = Nothing

      --left is sub, right is super
      --There really should be at most one existing sub and sup constraint
      addRecConstraint eM = do
        (subs,restCs) <- findRecordSubs <$> lookupConstraintsList vr
        let (sups,restCs') = findRecordSups restCs

            mergeSubs [] = return Nothing
            mergeSubs [m] = return $ Just m
            mergeSubs (m:ms) = Just <$> foldM (\(mp,t) (mp',t') -> do
                                                  mp'' <- unionWithM (\x y -> someLowerBound [x,y]) mp mp'
                                                  t'' <- someLowerBound [t,t']
                                                  return (mp'',t'')
                                              ) m ms
            mergeSups [] = return Nothing
            mergeSups [m] = return $ Just m
            mergeSups (m:ms) = Just <$> foldM (\(mp,t) (mp',t') -> do
                                                                mp'' <- intersectionWithM (\x y -> someUpperBound [x,y]) mp mp'
                                                                t'' <- someUpperBound [t,t']
                                                                return (mp'',t'')
                                                            ) m ms
        mbSub <- mergeSubs $ case eM of
                               Left mt -> mt : subs
                               _ -> subs
        mbSup <- mergeSups $ case eM of
                               Right mt -> mt : sups
                               _ -> sups
        normalizeConstraints =<< case (mbSub, mbSup) of
          (Just (subm,subt), Just (supm,supt)) -> do
            when (not $ M.isSubmapOfBy (\_ _ -> True) subm supm) $ tyErr2ext "Incompatible constraints" "Subtype constraint" "Supertype constraint" (JTRecord subt subm) (JTRecord supt supm)
            _ <- intersectionWithM (\x y -> y <: x) subm supm
            _ <- supt <: subt
            return $ Sub (JTRecord subt subm) : Super (JTRecord supt supm) : restCs'
          (Just (subm,subt), Nothing)  -> return $ Sub (JTRecord subt subm) : restCs'
          (Nothing , Just (supm,supt)) -> return $ Super (JTRecord supt supm) : restCs'
          _ -> return restCs'

      --There really should be at most one existing sub and sup constraint
      normalizeConstraints cl = putCs =<< cannonicalizeConstraints cl


cannonicalizeConstraints :: [Constraint] -> TMonad [Constraint]
cannonicalizeConstraints constraintList = do
        -- trace ("ccl: " ++ show constraintList) $ return ()
        let (subs,restCs)  = findForallSubs constraintList
            (sups,restCs') = findForallSups restCs
        mbSub <- mergeSubs subs
        mbSup <- mergeSups sups
        case (mbSub, mbSup) of
          (Just sub, Just sup) -> do
            sup <: sub
            return $ Sub sub : Super sup : restCs'
          (Just sub, Nothing)  -> return $ Sub sub : restCs'
          (Nothing , Just sup) -> return $ Super sup : restCs'
          _ -> return restCs'
  where

    findForallSubs cs = partitionOut go cs
      where go (Sub (JTForall vars t)) = Just (vars,t)
            go (Sub (JTFree _)) = Nothing
            go (Sub x) = Just ([],x)
            go _ = Nothing

    findForallSups cs = partitionOut go cs
      where go (Super (JTForall vars t)) = Just (vars,t)
            go (Super (JTFree _)) = Nothing
            go (Super x) = Just ([],x)
            go _ = Nothing

    findFuncs cs = partitionOut go cs
        where go (JTFunc args ret) = Just (args, ret)
              go _ = Nothing

    mergeSubs [] = return Nothing
    mergeSubs [([],t)] = return $ Just $ t
    mergeSubs [s] = return $ Just $ uncurry JTForall s
    mergeSubs ss = do
      rt <- newTyVar
      (_,frame) <- withLocalScope $ do
          instantiatedTypes <- mapM (uncurry instantiateScheme) ss
          let (funcTypes, otherTypes) = findFuncs instantiatedTypes
          when (not . null $ funcTypes) $ do
                     let (argss,rets) = unzip funcTypes
                         lft = length funcTypes
                     args' <- mapM someUpperBound $ filter ((== lft) . length) $ transpose argss
                     ret'  <- someLowerBound rets
                     rt <: JTFunc args' ret'
          mapM_ (rt <:) otherTypes
--        ((args',ret'):_,o:_) -> tyErr2ext "Incompatible Subtype Constraints" "Subtype constraint" "Subtype constraint" (JTFunc args' ret') o
      rt' <- resolveType rt
      case rt' of
        (JTFunc args res) -> do
          freeVarsInArgs <- S.unions <$> mapM freeVars args
          freeVarsInRes  <- freeVars res
          setFrozen $ frame `S.difference` (freeVarsInArgs `S.intersection` freeVarsInRes)
        _ -> setFrozen frame
      -- tryCloseFrozenVars
      Just <$> resolveType (JTForall (frame2VarRefs frame) rt')

    mergeSups [] = return Nothing
    mergeSups [([],t)] = return $ Just $ t
    mergeSups [s] = return $ Just $ uncurry JTForall s
    mergeSups ss = do
      rt <- newTyVar
      (_,frame) <- withLocalScope $ do
           instantiatedTypes <- mapM (uncurry instantiateScheme) ss
           let (funcTypes, otherTypes) = findFuncs instantiatedTypes
           when (not . null $ funcTypes) $ do
                     let (argss,rets) = unzip funcTypes
                     args' <- mapM someLowerBound $ transpose argss
                     ret'  <- someUpperBound rets
                     rt <: JTFunc args' ret'
           mapM_ (<: rt) otherTypes
--        ((args',ret'):_,o:_) -> tyErr2ext "Incompatible Supertype Constraints" "Supertype constraint" ("Supertype constraint" ++ show o) (JTFunc args' ret') o
      rt' <- resolveType rt

      case rt' of
        (JTFunc args res) -> do
          freeVarsInArgs <- S.unions <$> mapM freeVars args
          freeVarsInRes  <- freeVars res
          setFrozen $ frame `S.difference` (freeVarsInArgs `S.intersection` freeVarsInRes)
        _ -> setFrozen frame
      -- tryCloseFrozenVars
      Just <$> resolveType (JTForall (frame2VarRefs frame) rt')


tryCloseFrozenVars :: TMonad ()
tryCloseFrozenVars = runReaderT (loop . tc_frozen =<< get) []
    where
      loop frozen = do
        mapM_ go $ S.toList frozen
        newFrozen <- tc_frozen <$> lift get
        if S.null (newFrozen  `S.difference` frozen)
           then return ()
           else loop newFrozen
      go :: Int -> ReaderT [Either Int Int] TMonad ()
      go i = do
        s <- ask
        case findLoop i s of
          -- if no set is returned, then that means we just return (i.e. the constraint is dull)
          Just Nothing  -> return ()
          -- if a set is returned, then all vars in the set should be tied together
          Just (Just vrs) -> unifyGroup vrs
          Nothing       -> do
              t <- lift $ resolveTypeShallow (JTFree (Nothing, i))
              case t of
                (JTFree vr) -> do
                     l <- tryClose vr
                     case l of
                       [] -> return ()
                       cl -> do
                         mapM_ (go' vr) cl
                         lift (mapM_ (mapConstraint resolveType) cl)
                          -- not clear if we need to call again. if we resolve any constraints, they should point us back upwards...
                         --if cl remains free, recannonicalize it?
                _ -> return ()
      -- Left is super, right is sub
      go' (_,ref) (Sub (JTFree (_,i)))
          | i == ref = return ()
          | otherwise = -- trace (show ("super: " ++ show (ref,i))) $
                        local (\s -> Left ref : s) $ go i
      go' (_,ref) (Super (JTFree (_,i)))
          | i == ref = return ()
          | otherwise = -- trace (show ("sub: " ++ show (ref,i))) $
                        local (\s -> Right ref : s) $ go i
      go' _ _ = return ()

      unifyGroup :: [Int] -> ReaderT [Either Int Int] TMonad ()
      unifyGroup (vr:vrs) = lift $ mapM_ (\x -> instantiateVarRef (Nothing, x) (JTFree (Nothing,vr))) vrs
      unifyGroup [] = return ()

      findLoop i cs@(c:_) = go [] cs
          where
            cTyp = eitherIsLeft c
            go accum (r:rs)
               | either id id r == i && eitherIsLeft r == cTyp = Just $ Just (either id id r : accum)
                  -- i.e. there's a cycle to close
               | either id id r == i = Just Nothing
                  -- i.e. there's a "dull" cycle
               | eitherIsLeft r /= cTyp = Nothing -- we stop looking for a cycle because the chain is broken
               | otherwise = go (either id id r : accum) rs
            go _ [] = Nothing

      findLoop i [] = Nothing

      tryClose vr@(_,i) = do
        cl <- lift$ cannonicalizeConstraints =<< lookupConstraintsList vr
        -- trace ("tryclose: " ++ show vr) $ trace (show cl) $ return ()
        case partitionCs cl of
          (_,[s]) -> lift (instantiateVarRef vr s) >> go i >> return [] -- prefer lower bound (supertype constraint)
          ([s],_) -> lift (instantiateVarRef vr s) >> go i >> return []
          _ -> return cl

-- Manipulating the environment
withLocalScope :: TMonad a -> TMonad (a, Set Int)
withLocalScope act = do
  modify (\s -> s {tc_env   = M.empty : tc_env s,
                   tc_stack = S.empty : tc_stack s})
  res <- act
  frame <- head . tc_stack <$> get
  modify (\s -> s {tc_env   = drop 1 $ tc_env s,
                   tc_stack = drop 1 $ tc_stack s})
  return (res, frame)

setFrozen :: Set Int -> TMonad ()
setFrozen x = modify (\s -> s {tc_frozen = tc_frozen s `S.union` x})

-- addRefsToStack x = modify (\s -> s {tc_stack = F.foldr addToStack (tc_stack s) x })

frame2VarRefs :: Set t -> [(Maybe a, t)]
frame2VarRefs frame = (\x -> (Nothing,x)) <$> S.toList frame

addEnv :: Ident -> JType -> TMonad ()
addEnv ident typ = do
  envstack <- tc_env <$> get
  case envstack of
    (e:es) -> modify (\s -> s {tc_env = M.insert ident typ e : es}) -- we clobber/shadow var names
    _ -> throwError "empty env stack (this should never happen)"

newVarDecl :: Ident -> TMonad JType
newVarDecl ident = do
  v <- newTyVar
  addEnv ident v
  return v

resolveTypeGen :: ((JType -> TMonad JType) -> JType -> TMonad JType) -> JType -> TMonad JType
resolveTypeGen f typ = go typ
    where
      go :: JType -> TMonad JType
      go x@(JTFree (_, ref)) = do
        vars <- tc_vars <$> get
        case M.lookup ref vars of
          Just (SVType t) -> do
            res <- go t
            when (res /= t) $ modify (\s -> s {tc_vars = M.insert ref (SVType res) $ tc_vars s}) --mutation, shortcuts pointer chasing
            return res
          _ -> return x

      -- | Eliminates resolved vars from foralls, eliminates empty foralls.
      go (JTForall refs t) = do
        refs' <- catMaybes <$> mapM checkRef refs
        if null refs'
           then go t
           else JTForall refs' <$> go t
      go x = f go x

      checkRef x@(_, ref) = do
        vars <- tc_vars <$> get
        case M.lookup ref vars of
          Just (SVType t) -> return Nothing
          _ -> return $ Just x

resolveType = resolveTypeGen composOpM1
resolveTypeShallow = resolveTypeGen (const return)

--TODO create proper bounds for records
integrateLocalType :: JLocalType -> TMonad JType
integrateLocalType (env,typ) = do
  (r, frame) <- withLocalScope $ flip evalStateT M.empty $ do
                                 mapM_ integrateEnv env
                                 cloneType typ
  resolveType $ JTForall (frame2VarRefs frame) r
    where
      getRef (mbName, ref) = do
            m <- get
            case M.lookup ref m of
              Just newTy -> return newTy
              Nothing -> do
                newTy <- (\x -> JTFree (mbName, snd x)) <$> lift newVarRef
                put $ M.insert ref newTy m
                return newTy

      integrateEnv (vr,c) = do
        newTy <- getRef vr
        case c of
          (Sub t) -> lift . (newTy <:) =<< cloneType t
          (Super t) -> lift . (<: newTy) =<< cloneType t

      cloneType (JTFree vr) = getRef vr
      cloneType x = composOpM1 cloneType x

lookupEnv :: Ident -> TMonad JType
lookupEnv ident = resolveType =<< go . tc_env =<< get
    where go (e:es) = case M.lookup ident e of
                        Just t -> return t
                        Nothing -> go es
          go _ = tyErr0 $ "unable to resolve variable name: " ++ (show $ jsToDoc $ ident)


freeVars :: JType -> TMonad (Set Int)
freeVars t = execWriterT . go =<< resolveType t
    where go (JTFree (_, ref)) = tell (S.singleton ref)
          go x = composOpM1_ go x

--only works on resolved types
instantiateScheme :: [VarRef] -> JType -> TMonad JType
instantiateScheme vrs t = evalStateT (go t) M.empty
    where
      schemeVars = S.fromList $ map snd vrs
      go :: JType -> StateT (Map Int JType) TMonad JType
      go (JTFree vr@(mbName, ref))
          | ref `S.member` schemeVars = do
                       m <- get
                       case M.lookup ref m of
                         Just newTy -> return newTy
                         Nothing -> do
                           newRef <- (\x -> (mbName, snd x)) <$> lift newVarRef
                           put $ M.insert ref (JTFree newRef) m
                           mapM_ (lift . addConstraint newRef <=< mapConstraint go) =<< lift (lookupConstraintsList vr)
                           return (JTFree newRef)
      go x = composOpM1 go x

--only works on resolved types
instantiateRigidScheme :: [VarRef] -> JType -> TMonad JType
instantiateRigidScheme vrs t = evalStateT (go t) M.empty
    where
      schemeVars = S.fromList $ map snd vrs
      go :: JType -> StateT (Map Int JType) TMonad JType
      go (JTFree vr@(mbName, ref))
          | ref `S.member` schemeVars = do
                       m <- get
                       case M.lookup ref m of
                         Just newTy -> return newTy
                         Nothing -> do
                           newRef <- JTRigid vr . S.fromList <$> lift (lookupConstraintsList vr)
                           put $ M.insert ref newRef m
                           return newRef
      go x = composOpM1 go x

--only works on resolved types
checkEscapedVars :: [VarRef] -> JType -> TMonad ()
checkEscapedVars vrs t = go t
    where
      vs = S.fromList $ map snd vrs
      go t@(JTRigid (_,ref) _)
          | ref `S.member` vs = tyErr1 "Qualified var escapes into environment" t
          | otherwise = return ()
      go x = composOpM1_ go x

-- Subtyping
(<:) :: JType -> JType -> TMonad ()
x <: y = do
     xt <- resolveTypeShallow x --shallow because subtyping can close
     yt <- resolveTypeShallow y
     -- trace ("sub : " ++ show xt ++ ", " ++ show yt) $ return ()
     if xt == yt
        then return ()
        else go xt yt `withContext` (do
                                      xt <- prettyType x
                                      yt <- prettyType y
                                      return $ "When applying subtype constraint: " ++ xt ++ " <: " ++ yt)
  where

    go _ JTStat = return ()
    go JTImpossible _ = return ()

    go xt@(JTFree ref) yt@(JTFree ref2) = addConstraint ref  (Sub yt) >>
                                          addConstraint ref2 (Super xt)

    go (JTFree ref) yt = addConstraint ref (Sub yt)
    go xt (JTFree ref) = addConstraint ref (Super xt)

    --above or below jtfrees ?

    -- v <: \/ a. t --> v <: t[a:=x], x not in conclusion
    go xt yt@(JTForall vars t) = do
           t' <- instantiateRigidScheme vars t
           go xt t'
           checkEscapedVars vars =<< resolveType xt
           --then check that no fresh types appear in xt

    -- \/ a. t <: v --> [t] <: v
    go (JTForall vars t) yt = do
           t' <- instantiateScheme vars t
           go t' yt

    go xt@(JTFunc argsx retx) yt@(JTFunc argsy rety) = do
           -- TODO: zipWithM_ (<:) (appArgst ++ repeat JTStat) argst -- handle empty args cases
           when (length argsy < length argsx) $ tyErr2Sub xt yt
           zipWithM_ (<:) argsy argsx -- functions are contravariant in argument type
           retx <: rety -- functions are covariant in return type
    go (JTList xt) (JTList yt) = xt <: yt
    go (JTMap xt) (JTMap yt) = xt <: yt
    go (JTRecord xt xm) (JTRecord yt ym)
        | M.isSubmapOfBy (\_ _ -> True) ym xm = xt <: yt >> intersectionWithM (<:) xm ym >> return () --TODO not right?
    go xt yt = tyErr2Sub xt yt

(<<:>) :: TMonad JType -> TMonad JType -> TMonad ()
x <<:> y = join $ liftA2 (<:) x y

someUpperBound :: [JType] -> TMonad JType
someUpperBound [] = return JTStat
someUpperBound xs = do
  res <- newTyVar
  b <- (mapM_ (<: res) xs >> return True) `catchError` \e -> return False
  if b then return res else return JTStat

someLowerBound :: [JType] -> TMonad JType
someLowerBound [] = return JTImpossible
someLowerBound xs = do
  res <- newTyVar
  mapM_ (res <:) xs
  return res
--  b <- (mapM_ (res <:) xs >> return True) `catchError` \e -> return False
--  if b then return res else return JTImpossible

(=.=) :: JType -> JType -> TMonad JType
x =.= y = do
      x <: y
      y <: x
      return x

--todo difft ixing. a[b] --num lookup, a#[b] --string lookup, a.[b] --num literal lookup (i.e. tuple projection)

instance JTypeCheck JExpr where
    typecheck (ValExpr e) = typecheck e
    typecheck (SelExpr e (StrI i)) =
        do et <- typecheck e
           case et of
             (JTRecord t m) -> case M.lookup i m of
                               Just res -> return res
                               Nothing -> tyErr1 ("Record contains no field named " ++ show i) et -- record extension would go here
             (JTFree r) -> do
                            res <- newTyVar
                            addConstraint r (Sub (JTRecord res (M.singleton i res)))
                            return res
             _ -> tyErr1 "Cannot use record selector on this value" et
    typecheck (IdxExpr e e1) = undefined --this is tricky
    typecheck (InfixExpr s e e1)
        | s `elem` ["-","/","*"] = setFixed JTNum >> return JTNum
        | s == "+" = setFixed JTNum >> return JTNum -- `orElse` setFixed JTStr --TODO: Intersection types
        | s == "++" = setFixed JTString >> return JTString
        | s `elem` [">","<"] = setFixed JTNum >> return JTBool
        | s `elem` ["==","/="] = do
                            et <- typecheck e
                            e1t <- typecheck e1
                            _ <- et =.= e1t
                            return JTBool
        | s `elem` ["||","&&"] = setFixed JTBool >> return JTBool
        | otherwise = throwError $ "Unhandled operator: " ++ s
        where setFixed t = do
                  typecheck e  <<:> return t
                  typecheck e1 <<:> return t

    typecheck (PPostExpr _ _ e) = case e of
                                 (SelExpr _ _) -> go
                                 (ValExpr (JVar _)) -> go
                                 (IdxExpr _ _) -> go
                                 _ -> tyErr1 "Value not compatible with postfix assignment" =<< typecheck e
        where go = (typecheck e <<:> return JTNum) >> return JTNum

    typecheck (IfExpr e e1 e2) = do
                            typecheck e <<:> return JTBool
                            join $ liftA2 (\x y -> someUpperBound [x,y]) (typecheck e1) (typecheck e2)

    typecheck (NewExpr e) = undefined --yipe

    --when we instantiate a scheme, all the elements of the head
    --that are not in the tail are henceforth unreachable and can be closed
    --but that's just an optimization
    typecheck (ApplExpr e appArgse) = do
                            et <- typecheck e
                            appArgst <- mapM typecheck appArgse
                            let go (JTForall vars t) = go =<< instantiateScheme vars t
                                go (JTFunc argst rett) = do
                                        zipWithM_ (<:) (appArgst ++ repeat JTStat) argst
                                        return rett
                                go (JTFree vr) = do
                                        ret <- newTyVar
                                        addConstraint vr (Sub (JTFunc appArgst ret))
                                        return ret
                                go x = tyErr1 "Cannot apply value as function" x
                            go et


    typecheck (UnsatExpr _) = undefined --saturate (avoiding creation of existing ids) then typecheck
    typecheck (AntiExpr s) = error $ "Antiquoted expression not provided with explicit signature: " ++ show s

    --TODO: if we're typechecking a function, we can assign the types of the args to the given args
    typecheck (TypeExpr forceType e t)
              | forceType = integrateLocalType t
              | otherwise = do
                            t1 <- integrateLocalType t
                            typecheck e <<:> return t1
                            return t1

instance JTypeCheck JVal where
    typecheck (JVar i) =
        case i of
          StrI "true" -> return JTBool
          StrI "false" -> return JTBool
          StrI "null"  -> newTyVar
          StrI _ -> lookupEnv i

    typecheck (JInt _) = return JTNum
    typecheck (JDouble _) = return JTNum
    typecheck (JStr _) = return JTString
    typecheck (JList xs) = typecheck (JHash $ M.fromList $ zip (map show [(0::Int)..]) xs)
                           -- fmap JTList . someUpperBound =<< mapM typecheck xs
    typecheck (JRegEx _) = undefined --regex object
    typecheck (JHash mp) = do
      mp' <- T.mapM typecheck mp
      t <- if M.null mp'
             then return JTImpossible
             else someUpperBound $ M.elems mp'
      return $ JTRecord t mp'
    typecheck (JFunc args body) = do
          ((argst',res'), frame) <- withLocalScope $ do
                                      argst <- mapM newVarDecl args
                                      res <- typecheck body
                                      return (argst,res)
          rt <- resolveType $ JTFunc argst' res'
          freeVarsInArgs <- S.unions <$> mapM freeVars argst'
          freeVarsInRes  <- freeVars res'
          setFrozen $ frame `S.difference` (freeVarsInArgs `S.intersection` freeVarsInRes)
          tryCloseFrozenVars
          resolveType $ JTForall (frame2VarRefs frame) rt

    typecheck (UnsatVal _) = undefined --saturate (avoiding creation of existing ids) then typecheck

instance JTypeCheck JStat where
    typecheck (DeclStat ident Nothing) = newVarDecl ident >> return JTStat
    typecheck (DeclStat ident (Just t)) = integrateLocalType t >>= addEnv ident >> return JTStat
    typecheck (ReturnStat e) = typecheck e
    typecheck (IfStat e s s1) = do
                            typecheck e <<:> return JTBool
                            join $ liftA2 (\x y -> someUpperBound [x,y]) (typecheck s) (typecheck s1)
    typecheck (WhileStat _ e s) = do
                            typecheck e <<:> return JTBool
                            typecheck s
    typecheck (ForInStat _ _ _ _) = undefined -- yipe!
    typecheck (SwitchStat e xs d) = undefined -- check e, unify e with firsts, check seconds, take glb of seconds
                                    --oh, hey, add typecase to language!?
    typecheck (TryStat _ _ _ _) = undefined -- should be easy
    typecheck (BlockStat xs) = do
                            ts <- mapM typecheckWithBlock xs
                            someUpperBound $ stripStat ts
        where stripStat (JTStat:ts) = stripStat ts
              stripStat (t:ts) = t : stripStat ts
              stripStat t = t
    typecheck (ApplStat args body) = typecheck (ApplExpr args body) >> return JTStat
    typecheck (PPostStat b s e) = typecheck (PPostExpr b s e) >> return JTStat
    typecheck (AssignStat e e1) = do
      typecheck e1 <<:> typecheck e
      return JTStat
    typecheck (UnsatBlock _) = undefined --oyvey
    typecheck (AntiStat _) = undefined --oyvey
    typecheck (BreakStat _) = return JTStat
    typecheck (ForeignStat i t) = integrateLocalType t >>= addEnv i >> return JTStat

typecheckWithBlock :: (JsToDoc a, JMacro a, JTypeCheck a) => a -> TMonad JType
typecheckWithBlock stat = typecheck stat `withContext` (return $ "In statement: " ++ (T.unpack . displayT . renderCompact $ renderJs stat))
