{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Trie.Map.Hidden(
  -- * Types
  TMap(..),
  -- * Queries
  match,
  lookup,
  lookupPrefixes,
  member, notMember,
  null, count,
  keys, elems,
  -- * Construction
  empty, just,
  singleton,

  -- * Single item modification
  insertWith, insert,
  deleteWith, delete,

  adjust, revise, update, alter,

  -- * Combine
  union, unionWith,
  intersection, intersectionWith,
  difference, differenceWith,
  appendWith,

  -- * Conversion
  toList, fromList, fromListWith,
  toAscList, fromAscList, fromAscListWith,
  toMap, fromMap,
  keysTSet, fromTSet,

  -- * Parsing
  toParser, toParser_, toParser__,

  -- * Traversing with keys
  traverseWithKey, mapWithKey, foldMapWithKey, foldrWithKey,

  -- * Internals
  Node(..),
  foldTMap,
)
where

import           Prelude                hiding (lookup, null)

import           Data.Semigroup

import           Control.Applicative    hiding (empty)
import qualified Control.Applicative    as Ap (empty)
import           Control.Monad

import qualified Data.Foldable          as F
import qualified Data.List              as List (foldl')
import qualified Data.List.NonEmpty     as NE
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe, isJust, isNothing)

import           Data.Trie.Set.Internal (TSet (..))
import qualified Data.Trie.Set.Internal as TSet

import           Control.DeepSeq
import           Data.Functor.Classes
import qualified GHC.Exts
import           Text.Show (showListWith)

import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex

import Data.Hashable.Lifted
import Data.Hashable
import Witherable
import Data.These (These(..))
import Data.Zip (Zip(..))
import Data.Align ( Align(..), Semialign(..) )
import Data.Matchable

data Node c a r = Node !(Maybe a) !(Map c r)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Eq c, Eq a) => Eq1 (Node c a) where
  liftEq = liftEq2 (==)

instance (Ord c, Ord a) => Ord1 (Node c a) where
  liftCompare = liftCompare2 compare

instance Eq c => Eq2 (Node c) where
  liftEq2 eqA eqR (Node a1 e1) (Node a2 e2) = liftEq eqA a1 a2 && liftEq eqR e1 e2

instance Ord c => Ord2 (Node c) where
  liftCompare2 cmpA cmpR (Node a1 e1) (Node a2 e2) = liftCompare cmpA a1 a2 <> liftCompare cmpR e1 e2

instance (NFData c, NFData a, NFData r) => NFData (Node c a r) where
  rnf (Node a e) = rnf a `seq` rnf e

-- | Mapping from @[c]@ to @a@ implemented as a trie.
--   This type serves the almost same purpose of @Map [c] a@,
--   but can be looked up more efficiently.
newtype TMap c a = TMap { getNode :: Node c a (TMap c a) }
  deriving (Eq, Ord)

instance Show2 TMap where
  liftShowsPrec2 _ showListC showspA _ p t = showParen (p > 10) $
    showString "fromList " . showListWith (showPairWith showListC (showspA 0)) (toList t)

showPairWith :: (a -> ShowS) -> (b -> ShowS) -> (a,b) -> ShowS
showPairWith showsA showsB = liftShowsPrec2 (const showsA) (showListWith showsA) (const showsB) (showListWith showsB) 0

instance Show c => Show1 (TMap c) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show c, Show a) => Show (TMap c a) where
  showsPrec = showsPrec2

instance (NFData c, NFData a) => NFData (TMap c a) where
  rnf (TMap node) = rnf node

instance (Eq c) => Eq1 (TMap c) where
  liftEq = liftEq2 (==)

instance Eq2 TMap where
  liftEq2 eqC eqA = go
    where
      go (TMap (Node ma1 e1)) (TMap (Node ma2 e2)) =
        liftEq eqA ma1 ma2 &&
        liftEq2 eqC go e1 e2

instance (Ord c) => Ord1 (TMap c) where
  liftCompare cmp (TMap m1) (TMap m2) = liftCompare2 cmp (liftCompare cmp) m1 m2

instance Ord2 TMap where
  liftCompare2 cmpC cmpA = go
    where
      go (TMap (Node ma1 e1)) (TMap (Node ma2 e2)) =
        liftCompare cmpA ma1 ma2 <>
        liftCompare2 cmpC go e1 e2

instance (Ord c) => GHC.Exts.IsList (TMap c a) where
  type Item (TMap c a) = ([c],a)
  fromList = fromList
  toList = toList

instance Hashable2 TMap where
  liftHashWithSalt2 hashC hashA = hashT
    where
      hashMA = liftHashWithSalt hashA
      hashEdges = liftHashWithSalt2 hashC hashT
      hashT s (TMap (Node ma e)) = s `hashMA` ma `hashEdges` e

instance Hashable c => Hashable1 (TMap c) where
  liftHashWithSalt = liftHashWithSalt2 hashWithSalt

instance (Hashable c, Hashable a) => Hashable (TMap c a) where
  hashWithSalt = hashWithSalt2

instance FunctorWithIndex [c] (TMap c) where
  imap = mapWithKey

instance FoldableWithIndex [c] (TMap c) where
  ifoldr = foldrWithKey

instance TraversableWithIndex [c] (TMap c) where
  itraverse = traverseWithKey

instance Ord c => Filterable (TMap c) where
  mapMaybe f = go
    where
      go (TMap (Node ma edges)) =
        TMap (Node (ma >>= f) (mapMaybe (nonEmptyTMap . go) edges))

instance Ord c => Witherable (TMap c) where
  wither f = go
    where
      go (TMap (Node ma edges)) = fmap TMap $
        Node <$> wither f ma <*> wither (fmap nonEmptyTMap . go) edges

instance Ord c => FilterableWithIndex [c] (TMap c) where
  imapMaybe f (TMap (Node ma edges)) = TMap (Node mb edges')
    where
      mb = ma >>= f []
      edges' = imapMaybe (\c t -> nonEmptyTMap $ imapMaybe (f . (c:)) t) edges

instance Ord c => WitherableWithIndex [c] (TMap c) where
  iwither f (TMap (Node ma edges)) = TMap <$> (Node <$> mb <*> edges')
    where
      mb = wither (f []) ma
      edges' = iwither child edges
      child c t = nonEmptyTMap <$> iwither (f . (c :)) t

instance Ord c => Semialign (TMap c) where
  align (TMap (Node ma e1)) (TMap (Node mb e2)) = TMap (Node mc e')
    where
      mc = align ma mb
      e' = alignWith subtree e1 e2
      subtree (This t1) = This <$> t1
      subtree (That t2) = That <$> t2
      subtree (These t1 t2) = align t1 t2

instance (Ord c) => Align (TMap c) where
  nil = empty

instance (Ord c) => Zip (TMap c) where
  zipWith op = intersectionWith (\a b -> Just (op a b))

instance (Eq c) => Matchable (TMap c) where
  zipMatchWith f = go
    where
      go (TMap (Node ma e1)) (TMap (Node mb e2)) = TMap <$> (Node <$> mc <*> e')
        where
          mc = zipMatchWith f ma mb
          e' = zipMatchWith go e1 e2

-- * Queries

-- | Perform partial matching against a @TMap@.
--
--   @match xs tmap@ returns two values. The first value is the result of
--   'lookup'. The second is another @TMap@ for all keys which contain @xs@ as their prefix.
--   The keys of the returned map do not contain the common prefix @xs@.
--
-- ===== Example
-- 
-- >>> let x = fromList [("ham", 1), ("bacon", 2), ("hamburger", 3)]
-- >>> match "ham" x
-- (Just 1,fromList [("",1),("burger",3)])
match :: (Ord c) => [c] -> TMap c a -> (Maybe a, TMap c a)
match []     t@(TMap (Node ma _)) = (ma, t)
match (c:cs)   (TMap (Node _  e)) =
  case Map.lookup c e of
    Nothing -> (Nothing, empty)
    Just t' -> match cs t'

-- | @lookup xs tmap@ returns @Just a@ if @tmap@ contains mapping
--   from @xs@ to @a@, and returns @Nothing@ if not.
lookup :: (Ord c) => [c] -> TMap c a -> Maybe a
lookup cs = fst . match cs

-- | @lookupPrefixes xs tmap@ performs 'lookup' for every prefixes of the input string @xs@
--   and returns list of every pair of prefix and value exising in @tmap@.
--
-- ===== Example
-- 
-- >>> let x = fromList [("ham", 1), ("bacon", 2), ("hamburger", 3)]
-- >>> lookupPrefixes "hamburger and bacon" x
-- [("ham",1),("hamburger",3)]
lookupPrefixes :: (Ord c) => [c] -> TMap c a -> [([c], a)]
lookupPrefixes = go []
  where
    entry revPrefix ma = case ma of
      Nothing -> id
      Just a -> ((reverse revPrefix, a) :)
    
    go revPrefix [] (TMap (Node ma _)) = entry revPrefix ma []
    go revPrefix (x:xs) (TMap (Node ma e)) = entry revPrefix ma $
      case Map.lookup x e of
        Nothing -> []
        Just rest -> go (x : revPrefix) xs rest

member, notMember :: (Ord c) => [c] -> TMap c a -> Bool
member cs = isJust . lookup cs
notMember cs = isNothing . lookup cs

-- | Tests if given map is empty.
null :: TMap c a -> Bool
null (TMap (Node ma e)) = isNothing ma && Map.null e
{- Ensure all @TMap@ values exposed to users have no
   redundant node. -}

-- | Returns number of entries.
--
--   Note that this operation takes O(number of nodes),
--   unlike O(1) of 'Map.size'.
count :: TMap c a -> Int
count = foldTMap count'
  where
    count' (Node ma e) = F.foldl' (+) (length ma) e

-- | Returns list of key strings, in ascending order.
keys :: TMap c a -> [[c]]
keys = foldTMap keys'
  where
    keys' (Node ma e) =
      [ [] | isJust ma ] ++
      [ c:cs' | (c,css') <- Map.toList e, cs' <- css' ]

-- | Returns list of values, in ascending order by its key.
elems :: TMap c a -> [a]
elems = foldTMap elems'
  where
    elems' (Node ma e) = F.toList ma ++ F.foldr (++) [] e

-- * Construction

-- | Empty @TMap@.
empty :: TMap c a
empty = TMap (Node Nothing Map.empty)

-- | @TMap@ which contains only one entry from the empty string to @a@.
just :: a -> TMap c a
just a = TMap (Node (Just a) Map.empty)

-- | @singleton xs a@ is a @TMap@ which contains only one entry
--   from @xs@ to @a@.
singleton :: [c] -> a -> TMap c a
singleton cs a0 = foldr cons (just a0) cs

cons :: c -> TMap c a -> TMap c a
cons c t = TMap (Node Nothing (Map.singleton c t))

-- * Single-item modification

-- | Inserts an entry of key and value pair.
--
--   Already existing value will be overwritten.
--
--   > insert = 'insertWith' (const a)
insert :: (Ord c) => [c] -> a -> TMap c a -> TMap c a
insert cs a = revise (const a) cs

-- | Deletes an entry with given key.
--
--   > delete = 'update' (const Nothing)
delete :: (Ord c) => [c] -> TMap c a -> TMap c a
delete = update (const Nothing)

-- | @insertWith op xs a tmap@ inserts an entry of key-value pair @(cs,a)@
--   to the @tmap@. If @tmap@ already has an entry with key equals to
--   @xs@, its value @b@ is replaced with @op a b@.
--
--   > insertWith op cs a = 'revise' (maybe a (op a)) cs
insertWith :: (Ord c) => (a -> a -> a) -> [c] -> a -> TMap c a -> TMap c a
insertWith f cs a = revise (maybe a (f a)) cs

-- | Deletes an entry with given key, conditionally.
--
--   @deleteWith f xs b@ looks up an entry with key @xs@, and if such entry
--   is found, evaluate @f b a@ with its value @a@. If it returned @Nothing@,
--   the entry is deleted. Otherwise, if it returned @Just a'@, the value of
--   the entry is replaced with @a'@.
--
--   > deleteWith f cs b = 'update' (f b) cs
deleteWith :: (Ord c) => (b -> a -> Maybe a) -> [c] -> b -> TMap c a -> TMap c a
deleteWith f cs b = update (f b) cs

-- | Apply a function to the entry with given key.
adjust :: (Ord c) => (a -> a) -> [c] -> TMap c a -> TMap c a
adjust f = F.foldr step base
  where
    base (TMap (Node ma e)) = TMap (Node (f <$> ma) e)
    step x xs (TMap (Node ma e)) =
      let e' = Map.adjust xs x e
      in TMap (Node ma e')
{-# INLINE adjust #-}

-- | Apply a function @f@ to the entry with the given key. If there is no such
--   entry, insert an entry with value @f Nothing@.
revise :: (Ord c) => (Maybe a -> a) -> [c] -> TMap c a -> TMap c a
revise f = fst . F.foldr step (base, just (f Nothing))
  where
    base (TMap (Node ma e)) = TMap (Node (Just (f ma)) e)
    step x (inserter', xs') =
      let inserter (TMap (Node ma e)) =
            let e' = Map.insertWith (const inserter') x xs' e
            in TMap (Node ma e')
      in (inserter, cons x xs')
{-# INLINE revise #-}

-- | Apply a function @f@ to the entry with given key. If @f@ returns
--   @Nothing@, that entry is deleted.
update :: (Ord c) => (a -> Maybe a) -> [c] -> TMap c a -> TMap c a
update f cs = fromMaybe empty . update_ f cs
{-# INLINE update #-}

update_ :: (Ord c) => (a -> Maybe a) -> [c] -> TMap c a -> Maybe (TMap c a)
update_ f = F.foldr step base
  where
    base (TMap (Node ma e)) =
      let ma' = ma >>= f
      in if isNothing ma' && Map.null e
           then Nothing
           else Just $ TMap (Node ma' e)
    step x xs (TMap (Node ma e)) =
      let e' = Map.update xs x e
      in if isNothing ma && Map.null e'
           then Nothing
           else Just $ TMap (Node ma e')
{-# INLINE update_ #-}

-- | Apply a function @f@ to the entry with given key. This function @alter@
--   is the most generic version of 'adjust', 'revise', 'update'.
-- 
--   * You can insert new entry by returning @Just a@ from @f Nothing@.
--   * You can delete existing entry by returning @Nothing@ from
--     @f (Just a)@.
--
--   This function always evaluates @f Nothing@ in addition to determine
--   operation applied to the given key.
--   If you're not going to use @alter@ on missing keys, consider using @update@ instead.
alter :: (Ord c) => (Maybe a -> Maybe a) -> [c] -> TMap c a -> TMap c a
alter f =
  case f Nothing of
    Nothing -> update (f . Just)
    Just f0 -> \cs -> fromMaybe empty . alter_ f f0 cs
{-# INLINE alter #-}

alter_ :: (Ord c) => (Maybe a -> Maybe a) -> a -> [c] -> TMap c a -> Maybe (TMap c a)
alter_ f f0 = fst . F.foldr step (base, just f0)
  where
    base (TMap (Node ma e)) =
      let ma' = f ma
      in if isNothing ma' && Map.null e
           then Nothing
           else Just $ TMap (Node ma' e)
    step x (alterer', xs') =
      let alterer (TMap (Node ma e)) =
            let e' = Map.alter (maybe (Just xs') alterer') x e
            in if isNothing ma && Map.null e'
                 then Nothing
                 else Just $ TMap (Node ma e')
      in (alterer, cons x xs')
{-# INLINE alter_ #-}

-- * Combine
union :: (Ord c) => TMap c a -> TMap c a -> TMap c a
union = unionWith const

unionWith :: (Ord c) => (a -> a -> a) -> TMap c a -> TMap c a -> TMap c a
unionWith f = go
  where
    go (TMap (Node mat et)) (TMap (Node mau eu)) =
      let maz = case (mat, mau) of
            (Nothing, Nothing) -> Nothing
            (Just at, Nothing) -> Just at
            (Nothing, Just au) -> Just au
            (Just at, Just au) -> Just (f at au)
          ez = Map.unionWith go et eu
      in TMap (Node maz ez)

intersection :: (Ord c) => TMap c a -> TMap c b -> TMap c a
intersection = intersectionWith (\a _ -> Just a)

intersectionWith :: (Ord c) =>
  (a -> b -> Maybe r) -> TMap c a -> TMap c b -> TMap c r
intersectionWith f x y = fromMaybe empty $ go x y
  where
    go (TMap (Node ma ex)) (TMap (Node mb ey)) =
      if isNothing mr && Map.null ez
        then Nothing
        else Just $ TMap (Node mr ez)
      where
        mr = do a <- ma
                b <- mb
                f a b
        emz = Map.intersectionWith go ex ey
        ez = Map.mapMaybe id emz

difference :: (Ord c) => TMap c a -> TMap c b -> TMap c a
difference = differenceWith (\_ _ -> Nothing)

differenceWith :: (Ord c) =>
  (a -> b -> Maybe a) -> TMap c a -> TMap c b -> TMap c a
differenceWith f x y = fromMaybe empty $ go x y
  where
    go (TMap (Node ma ex)) (TMap (Node mb ey)) =
      if isNothing mr && Map.null ez
        then Nothing
        else Just $ TMap (Node mr ez)
      where
        mr = case (ma, mb) of
          (Nothing, _)       -> Nothing
          (Just a,  Nothing) -> Just a
          (Just a,  Just b)  -> f a b
        ez = Map.differenceWith go ex ey

{- |
Creates a new @TMap@ from two @TMap@s. The keys of the new map
are concatenations of one key from the first map and another one from the second map.

Corresponding values for these keys are calculated with the given function
of type @(x -> y -> z)@. If two different concatenations yield
the same key, the calculated values for these keys are combined with the 'Semigroup' operation @<>@.

The behavior of @appendWith@ is equivalent to the following implementation.

@
appendWith :: (Ord c, Semigroup z) => (x -> y -> z) ->
  TMap c x -> TMap c y -> TMap c z
appendWith f x y = 'fromListWith' (flip (<>))
  [ (kx ++ ky, f valx valy)
    | (kx, valx) <- 'toAscList' x
    , (ky, valy) <- toAscList y ]
@

In other words, a set of colliding key-valur pairs is combined in increasing order of the left key.
For example, suppose @x, y@ are @TMap@ with these key-value pairs,
and @kx1 ++ ky3, kx2 ++ ky2, kx3 ++ ky1@ are all equal to the same key @kz@.

@
x = 'fromAscList' [ (kx1, x1), (kx2, x2), (kx3, x3) ] -- kx1 < kx2 < kx3
y = fromAscList [ (ky1, y1), (ky2, y2), (ky3, y3) ]
@

On these maps, @appendWith@ combines the values for these colliding keys
in the order of @kx*@.

@
'lookup' kz (appendWith f x y) == Just (f x1 y3 <> f x2 y2 <> f x3 y1)
@

===== Example

> let x = fromList [("a", 1), ("aa", 2)]     :: TMap Char Int
>     y = fromList [("aa", 10), ("aaa", 20)] :: TMap Char Int
>
> appendWith (\a b -> show (a,b)) x y ==
>   fromList [ ("aaa", "(1,10)")
>            , ("aaaa", "(1,20)" <> "(2,10)")
>            , ("aaaaa", "(2,20)") ]

-}
appendWith :: (Ord c, Semigroup z) => (x -> y -> z) ->
  TMap c x -> TMap c y -> TMap c z
appendWith f xs (TMap (Node my ey))
  | Map.null ey = case my of
      Nothing -> empty
      Just y  -> fmap (`f` y) xs
  | otherwise = go xs
    where
      go (TMap (Node Nothing ex)) = TMap (Node Nothing (Map.map go ex))
      go (TMap (Node (Just x) ex)) =
        let mz = f x <$> my
            ex' = Map.map go ex
            ey' = Map.map (fmap (f x)) ey
            ez = Map.unionWith (unionWith (<>)) ey' ex'
        in TMap (Node mz ez)

-- * Instances

instance Functor (TMap c) where
  fmap f = go
    where
      go (TMap (Node ma e)) = TMap (Node (fmap f ma) (Map.map go e))

instance Foldable (TMap c) where
  foldr f z = foldr f z . elems
  toList = elems
  null = Data.Trie.Map.Hidden.null
  length = count

instance Traversable (TMap c) where
  traverse f = traverseWithKey (const f)

-- | 'unionWith'-based
instance (Ord c, Semigroup a) => Semigroup (TMap c a) where
  (<>) = unionWith (<>)
  stimes n = fmap (stimes n)

-- | 'unionWith'-based
instance (Ord c, Semigroup a) => Monoid (TMap c a) where
  mempty = empty
  mappend = (<>)

-- * Conversion

toList :: TMap c a -> [([c], a)]
toList = foldrWithKey (\k a r -> (k,a) : r) []

fromList :: Ord c => [([c], a)] -> TMap c a
fromList = List.foldl' (flip (uncurry insert)) empty

fromListWith :: Ord c => (a -> a -> a) -> [ ([c],a)] -> TMap c a
fromListWith op = List.foldl' (flip (uncurry (insertWith op))) empty

toAscList :: TMap c a -> [([c], a)]
toAscList = toList

fromAscList :: Eq c => [([c], a)] -> TMap c a
fromAscList [] = empty
fromAscList [(cs, a)] = singleton cs a
fromAscList pairs =
  let (as, gs) = group_ pairs
      ma = NE.last <$> NE.nonEmpty as
      e = Map.fromDistinctAscList $ map (fmap fromAscList) gs
  in TMap (Node ma e)

foldl1' :: (a -> a -> a) -> NE.NonEmpty a -> a
foldl1' f (a NE.:| as) = F.foldl' f a as

fromAscListWith :: Ord c => (a -> a -> a) -> [ ([c],a)] -> TMap c a
fromAscListWith _ [] = empty
fromAscListWith op pairs =
  let (as, gs) = group_ pairs
      ma = foldl1' (flip op) <$> NE.nonEmpty as
      e = Map.fromDistinctAscList $ map (fmap (fromAscListWith op)) gs
  in TMap (Node ma e)

group_ :: Eq c => [([c], a)] -> ([a], [ (c, [ ([c], a) ]) ] )
group_ = foldr step ([], [])
  where
    step ([], a) ~(as, gs) = (a : as, gs)
    step (c:cs, a) ~(as, gs) = (as, prepend c cs a gs)
    
    prepend c cs a gs = case gs of
      (d,ps'):rest | c == d  -> (d, (cs,a):ps'):rest
      _                      -> (c, [(cs,a)]):gs

toMap :: TMap c a -> Map [c] a
toMap = Map.fromDistinctAscList . toAscList

fromMap :: (Eq c) => Map [c] a -> TMap c a
fromMap = fromAscList . Map.toAscList

keysTSet :: TMap c a -> TSet c
keysTSet (TMap (Node ma e)) =
    TSet (TSet.Node (isJust ma) (Map.map keysTSet e))

fromTSet :: ([c] -> a) -> TSet c -> TMap c a
fromTSet f = go []
  where
    go q (TSet (TSet.Node a e)) =
      let e' = Map.mapWithKey (\c -> go (c:q)) e
          a' = if a then Just (f (reverse q)) else Nothing
      in TMap (Node a' e')

-- * Parsing

toParser :: Alternative f =>
     (c -> f c') -- ^ char
  -> f eot       -- ^ eot
  -> TMap c a -> f ([c'], a)
toParser f eot = foldTMap toParser'
  where
    toParser' (Node ma e) =
      maybe Ap.empty (\a -> ([], a) <$ eot) ma <|>
      F.asum [ consFst <$> f c <*> p' | (c, p') <- Map.toAscList e ]

    consFst c (cs, a) = (c:cs, a)

toParser_ :: Alternative f =>
     (c -> f c') -- ^ char
  -> f eot       -- ^ eot
  -> TMap c a -> f a
toParser_ f eot = foldTMap toParser'
  where
    toParser' (Node ma e) =
      maybe Ap.empty (<$ eot) ma <|>
      F.asum [ f c *> p' | (c, p') <- Map.toAscList e ]

toParser__ :: Alternative f =>
     (c -> f c') -- ^ char
  -> f eot       -- ^ eot
  -> TMap c a -> f ()
toParser__ f eot = void . toParser_ f eot

-- * Traversing with keys

-- | Same semantics to following defintion, but have
--   more efficient implementation.
--
-- > traverseWithKey f = fmap fromAscList .
-- >                     traverse (\(cs,a) -> (,) cs <$> f cs a) .
-- >                     toAscList
traverseWithKey :: (Applicative f) =>
  ([c] -> a -> f b) -> TMap c a -> f (TMap c b)
traverseWithKey f (TMap (Node Nothing e)) = TMap . Node Nothing <$> Map.traverseWithKey (\c t' -> traverseWithKey (f . (c:)) t') e
traverseWithKey f (TMap (Node (Just a) e)) = fmap TMap $ Node <$> (Just <$> f [] a) <*> Map.traverseWithKey (\c t' -> traverseWithKey (f . (c:)) t') e

-- | Same semantics to following defintion, but have
--   more efficient implementation.
--
-- > mapWithKey f = fromAscList .
-- >                map (\(cs,a) -> (cs,  f cs a)) .
-- >                toAscList
mapWithKey :: ([c] -> a -> b) -> TMap c a -> TMap c b
mapWithKey f (TMap (Node ma e)) = TMap $ Node (f [] <$> ma) (Map.mapWithKey (\c t' -> mapWithKey (f . (c:)) t') e)

-- | Same semantics to following defintion, but have
--   more efficient implementation.
--
-- > foldMapWithKey f = foldMap (uncurry f) . toAscList
foldMapWithKey :: (Monoid r) => ([c] -> a -> r) -> TMap c a -> r
foldMapWithKey f = foldrWithKey (\k v r -> f k v <> r) mempty

-- | Same semantics to following defintion, but have
--   more efficient implementation.
--
-- > foldrWithKey f z = foldr (uncurry f) z . toAscList
foldrWithKey :: ([c] -> a -> r -> r) -> r -> TMap c a -> r
foldrWithKey f z (TMap (Node ma e)) =
  case ma of
    Nothing -> r
    Just a  -> f [] a r
  where
    r = Map.foldrWithKey (\c subTrie s ->
          foldrWithKey (f . (c:)) s subTrie) z e

-- * Other operations

foldTMap :: (Node c a r -> r) -> TMap c a -> r
foldTMap f = go
  where
    -- Use lazy @<$>@
    go (TMap (Node a e)) = f (Node a (go <$> e))

nonEmptyTMap :: TMap c a -> Maybe (TMap c a)
nonEmptyTMap t
  | null t = Nothing
  | otherwise = Just t
