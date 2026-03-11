{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.REST.Internal.WQO (
      empty
    , insert
    , insertMaybe
    , orderings
    , getRelation
    , merge
    , mergeAll
    , notStrongerThan
    , WQO
    , QORelation(..)
    , ExtendOrderingResult(..)
    , relevantTo
    , singleton
    , null
    , getPO
    , getECs
    , elems) where

import Prelude hiding (null, EQ, GT)
import GHC.Generics (Generic)
import qualified Data.Map as M
import Control.Monad
import Data.Hashable
import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S

import qualified Language.REST.Internal.EquivalenceClass as EC
import qualified Language.REST.Internal.PartialOrder as PO
import Language.REST.Internal.Orphans ()
import Language.REST.Op
import Language.REST.SMT

type PartialOrder     = PO.PartialOrder
type EquivalenceClass = EC.EquivalenceClass

data QORelation = QGT | QEQ deriving (Ord, Eq, Generic, Hashable)

instance Show QORelation where
  show QGT = ">"
  show QEQ = "≈"

instance {-# OVERLAPPING #-} ToSMTVar a Int => ToSMT (WQO a) Bool where
  toSMT (WQO ecs po) = And $ ecsSMT ++ posSMT where

    toSMT' :: a -> SMTExpr Int
    toSMT' = toSMT

    ecsSMT = do
      ec <- S.toList ecs
      let ecl = EC.toList ec
      guard $ length ecl >= 2
      return $ Equal (map toSMT' ecl)

    posSMT = do
      (ec, vs) <- PO.toDescsList po
      var        <- S.toList vs
      return $ Greater (toSMT $ EC.head ec) (toSMT $ EC.head var)


getPO :: WQO a -> PartialOrder (EquivalenceClass a)
getPO (WQO _ po)  = po

getECs :: WQO a -> S.Set (EquivalenceClass a)
getECs (WQO ecs _) = ecs

-- | Well-founded reflexive partial orders
data WQO a =
  -- Invariant: the first set contains all equivalence classes
  --
  -- The strict partial order describes the ordering of the
  -- equivalence classes in the first set.
  WQO (S.Set (EquivalenceClass a)) (PartialOrder (EquivalenceClass a))
  deriving (Ord, Eq, Generic, Hashable)

instance (Show a, Eq a, Hashable a) => Show (WQO a) where
    show (WQO ecs _)  | S.null ecs  = "⊤"
    show (WQO ecs po) = L.intercalate " ∧ " (map show ecs' ++ po')
        where
            ecs'          = filter (not . EC.isSingleton) $ S.toList ecs
            po'           =
                [show po | not (PO.isEmpty po)]
            --         else [show $ PO.mapUnsafe ecHead po]
            -- ecHead (x, y) = (EC.head x, EC.head y)

null :: Eq a => WQO a -> Bool
null wqo = wqo == empty

empty :: WQO a
empty = WQO S.empty PO.empty

singleton :: (Ord a, Eq a, Hashable a) => (a, a, QORelation) -> Maybe (WQO a)
singleton = insertMaybe empty

{-# INLINE elems #-}
elems :: (Ord a) => WQO a -> S.Set a
elems (WQO ec _) = S.unions $ map EC.elems (S.toList ec)

-- | @getEquivalenceClasses (>=) a b@ retrieves the equivanlence classes of
-- @a@ and @b@.
--
-- TODO: Why are these looked up in pairs and not individually?
{-# INLINE getEquivalenceClasses #-}
getEquivalenceClasses :: (Ord a, Eq a, Hashable a) => WQO a -> a -> a
  -> (Maybe (EquivalenceClass a), Maybe (EquivalenceClass a))
getEquivalenceClasses (WQO classes _) source target = (t, u)
  where
    t = L.find (EC.isMember source) classes'
    u = L.find (EC.isMember target) classes'
    classes' = S.toList classes

-- | Like @getEquivalenceClasses@ but only yields a result
-- if classes of equivalence are found for both elements.
{-# INLINE getEquivalenceClasses' #-}
getEquivalenceClasses'
  :: (Ord a, Hashable a)
  => WQO a
  -> a
  -> a
  -> Maybe (EC.EquivalenceClass a, EC.EquivalenceClass a)
getEquivalenceClasses' (WQO classes _) source target =
  do
    t <- L.find (EC.isMember source) classes'
    if EC.isMember target t
      then return (t, t)
      else (t,) <$> L.find (EC.isMember target) classes'
  where
    classes' = S.toList classes

-- | @getRelation (>=) a b == QEQ@ iff @a >= b@
--   @getRelation (>=) a b == QGT@ iff @a > b@
{-# INLINE getRelation #-}
getRelation :: (Ord a, Eq a, Hashable a) => WQO a -> a -> a -> Maybe QORelation
getRelation _ f g | f == g = Just QEQ
getRelation wqo@(WQO _ po) source target
    | Just (s, t) <- getEquivalenceClasses' wqo source target
    = if s == t
        then Just QEQ
        else
            if PO.gt po s t
                then Just QGT
                else Nothing
    | otherwise = Nothing

-- | @expandEC (>=) ec x@ adds an element @x@ to the equivalence class
-- @ec@ of @(>=)@.
expandEC :: (Ord a, Eq a, Hashable a) => WQO a -> EquivalenceClass a -> a -> WQO a
expandEC (WQO ecs po) ec x = WQO ecs' po'
    where
        ec'  = EC.insert x ec
        ecs' = S.insert ec' $ S.delete ec ecs
        po'  = PO.replaceUnsafe [ec] ec' po

-- | @mergeECs (>=) ec1 ec2@ combines the equivalence classes @ec1@ and @ec2@
-- of @(>=)@.
mergeECs :: (Ord a, Eq a, Hashable a) => WQO a -> EquivalenceClass a -> EquivalenceClass a -> WQO a
mergeECs (WQO ecs po) ec1 ec2 = WQO ecs' po'
    where
        ec'  = EC.union ec1 ec2
        ecs' = S.insert ec' $ S.delete ec2 $ S.delete ec1 ecs
        po'  = PO.replaceUnsafe [ec1, ec2] ec' po

type ECMap a = M.Map (EquivalenceClass a) (EquivalenceClass a)

{-# SPECIALISE notStrongerThan :: WQO Op -> WQO Op -> Bool #-}
-- |  @w1 `notStrongerThan` w2@ if it is possible to extend @w1@ with additional
--    relations to obtain @w2@
notStrongerThan :: forall a . (Ord a, Eq a, Hashable a) => WQO a -> WQO a -> Bool
notStrongerThan w1 w2 | w1 == w2 = True
notStrongerThan (WQO ecs po) (WQO ecs' po') = result where
  result = case mkEcsMap M.empty (S.toList ecs) of
    Just ecsMap -> all (gt ecsMap) (PO.toDescsList po)
    Nothing     -> False

  mkEcsMap :: ECMap a -> [EquivalenceClass a] -> Maybe (ECMap a)
  mkEcsMap buf []        = Just buf
  mkEcsMap buf (ec:rest) =
    do
      ec' <- L.find (ec `EC.isSubsetOf`) (S.toList ecs')
      mkEcsMap (M.insert ec ec' buf) rest
  gt ecsMap (ec, descs) =
    let
      Just ec' = M.lookup ec ecsMap
    in
      descs `S.isSubsetOf` PO.descendents ec' po'




mergeAll :: forall a. (Show a, Ord a, Eq a, Hashable a) => [WQO a] -> Maybe (WQO a)
mergeAll []            = Just empty
mergeAll [x]           = Just x
mergeAll (x : x' : xs) = do
  y <- merge x x'
  mergeAll (y : xs)

trace' :: String -> a -> a
trace' _ x = x

{-# INLINE merge #-}
merge :: forall a. (Ord a, Eq a, Hashable a) => WQO a -> WQO a -> Maybe (WQO a)
merge lhs@(WQO ecs po) rhs@(WQO ecs' po') | S.disjoint (elems lhs) (elems rhs)
  = Just $ WQO (S.union ecs ecs') (PO.unionDisjointUnsafe po po')
merge lhs rhs  =
  if S.size (elems lhs) >= S.size (elems rhs)
  then merge' lhs rhs
  else merge' rhs lhs

{-# SPECIALISE merge' :: WQO Op -> WQO Op -> Maybe (WQO Op) #-}
merge' :: forall a. (Ord a, Eq a, Hashable a) => WQO a -> WQO a -> Maybe (WQO a)
merge' lhs rhs@(WQO ecs po) = trace' message result where

    message = "Merge " ++ show (hash lhs) ++ " " ++ show (hash rhs)

    withEQs' = go lhs ecsFacts

    result = do
      withEQs <- withEQs'
      go withEQs poFacts

    ecsFacts :: [(a, a, QORelation)]
    ecsFacts = concatMap ecFacts (S.toList ecs)

    ecFacts ec =
        let
            xs = EC.toList ec
        in
            zipWith (\ a b -> (a, b, QEQ)) xs (tail xs)

    poFacts :: [(a, a, QORelation)]
    poFacts =
        map (\(a, b) -> (head (EC.toList a), head (EC.toList b), QGT)) (PO.toList po)

    go r []       = Just r
    go r (x : xs) =
      do
        r' <- insertMaybe r x
        go r' xs


data ExtendOrderingResult a =
    ValidExtension (WQO a)
  | AlreadyImplied
  | Contradicts

-- | @relevantTo wqo as bs@ returns a new WQO that contains only the necessary
--   relations to relate elements from @as@ with elements in @bs@ as they are
--   related in @wqo@.
relevantTo :: (Ord a, Eq a, Hashable a) => WQO a -> S.Set a -> S.Set a -> WQO a
relevantTo wqo0 as bs = go empty cartesianProduct where

  cartesianProduct = do
    x <- S.toList as
    y <- S.toList bs
    return (x, y)

  get _ (ValidExtension w) = w
  get w AlreadyImplied     = w
  get _ _                  = undefined

  go wqo []                     = wqo
  go wqo ((f, g) : xs) | f == g = go wqo xs
  go wqo ((f, g) : xs) | Just r  <- getRelation wqo0 f g
                       , wqo'    <- get wqo $ insert wqo (f, g, r)
                       = go wqo' xs
  go wqo ((f, g) : xs) | Just r  <- getRelation wqo0 g f
                       , wqo'    <- get wqo $ insert wqo (g, f, r)
                       = go wqo' xs
  go wqo (_ : xs)       = go wqo xs

{-# INLINE insertMaybe #-}
{-# SPECIALISE insertMaybe :: WQO Op -> (Op, Op, QORelation) -> Maybe (WQO Op) #-}
insertMaybe :: (Ord a, Eq a, Hashable a) => WQO a -> (a, a, QORelation) -> Maybe (WQO a)
insertMaybe wqo t = case insert wqo t of
  ValidExtension wqo' -> Just wqo'
  AlreadyImplied      -> Just wqo
  Contradicts         -> Nothing



{-# SPECIALISE insert :: WQO Op -> (Op, Op, QORelation) -> ExtendOrderingResult Op #-}
insert :: (Ord a, Eq a, Hashable a) => WQO a -> (a, a, QORelation) -> ExtendOrderingResult a
insert _   (f, g, QGT)  | f == g = Contradicts
insert wqo (f, g, r)    | Just r' <- getRelation wqo f g
                        = if r == r' then AlreadyImplied else Contradicts
insert wqo (f, g, _)    | isJust $ getRelation wqo g f = Contradicts

insert wqo@(WQO ecs po) (f, g, QEQ) = ValidExtension $
    case getEquivalenceClasses wqo f g of
        (Nothing, Nothing) ->
            let
                ecs' = S.insert (EC.fromList [f, g]) ecs
            in
                WQO ecs' po
        (Just ec, Nothing)   -> expandEC wqo ec g
        (Nothing, Just ec)   -> expandEC wqo ec f
        (Just ec1, Just ec2) -> mergeECs wqo ec1 ec2

insert wqo@(WQO ecs po) (f, g, QGT) = ValidExtension $
    case getEquivalenceClasses wqo f g of
        (Nothing, Nothing) ->
            let
                f'       = EC.singleton f
                g'       = EC.singleton g
                ecs'     = S.insert f' $ S.insert g' ecs
                Just po' = PO.insert po f' g'
            in
                WQO ecs' po'
        (Just ec, Nothing)   ->
            let
                g'       = EC.singleton g
                ecs'     = S.insert g' ecs
                Just po' = PO.insert po ec g'
            in
                WQO ecs' po'

        (Nothing, Just ec) ->
            let
                f'       = EC.singleton f
                ecs'     = S.insert f' ecs
                Just po' = PO.insert po f' ec
            in
                WQO ecs' po'
        (Just ec1, Just ec2) ->
            WQO ecs (PO.insertUnsafe po ec1 ec2)

-- | Generates all the possible orderings of the elements in the given set.
orderings :: forall a. (Ord a, Eq a, Hashable a) => S.Set a -> S.Set (WQO a)
orderings ops = go S.empty (S.singleton empty) where

  insert' w t | ValidExtension w' <- insert w t = Just w'
  insert' _ _                                   = Nothing

  go :: S.Set (WQO a) -> S.Set (WQO a) -> S.Set (WQO a)
  go seen acc | S.null acc = seen
  go seen acc =
    let
      ordering  = head $ S.toList acc
      acc'      = S.delete ordering acc
      seen'     = S.insert ordering seen
      newOrderings =
        S.fromList $ do
          f <- S.toList ops
          g <- S.toList (S.delete f ops)
          o <- [QEQ, QGT]
          maybeToList (insert' ordering (f,g, o))
      newOrderings' = S.difference newOrderings seen
    in
      go seen' (S.union acc' newOrderings')
