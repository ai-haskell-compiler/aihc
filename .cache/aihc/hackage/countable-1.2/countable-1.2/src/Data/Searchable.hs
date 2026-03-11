module Data.Searchable
    ( Searchable(..)
    , forsome
    , forevery
    , Finite(..)
    , finiteSearch
    , finiteCountPrevious
    , finiteCountMaybeNext
    ) where

import Control.Applicative
import Data.Countable
import Data.Int
import Data.List
import Data.Maybe
import Data.Traversable
import Data.Void
import Data.Word
import Prelude

-- | It turns out there are 'Searchable' instances that are not 'Finite'.
-- The @(c -> s)@ instance is based on the algorithm at
-- <http://math.andrej.com/2007/09/28/seemingly-impossible-functional-programs/>.
class Searchable a where
    search :: (a -> Maybe b) -> Maybe b

forsome :: (Searchable a) => (a -> Bool) -> Bool
forsome =
    isJust .
    search .
    (\ab a ->
         if ab a
             then Just ()
             else Nothing)

forevery :: (Searchable a) => (a -> Bool) -> Bool
forevery p = not (forsome (not . p))

instance (Searchable a) => Searchable (Maybe a) where
    search mamb =
        case mamb Nothing of
            Just b -> Just b
            Nothing -> search (mamb . Just)

instance (Searchable a, Searchable b) => Searchable (Either a b) where
    search eabb =
        case search (eabb . Left) of
            Just b -> Just b
            _ -> search (eabb . Right)

instance (Searchable a, Searchable b) => Searchable (a, b) where
    search abb = search (\a -> search (\b -> abb (a, b)))

instance (Countable c, Searchable s) => Searchable (c -> s) where
    search csmx =
        case search Just of
            Just def -> let
                prepend s cs c =
                    case countPrevious c of
                        Just c' -> cs c'
                        Nothing -> s
                -- | prepend :: s -> (c -> s) -> c -> s
                -- | findcs :: ((c -> s) -> Maybe x) -> c -> s
                findcs csm = let
                    mx =
                        search
                            (\s' -> do
                                 _ <- search (csm . (prepend s'))
                                 return s')
                    s =
                        case mx of
                            Just s' -> s'
                            _ -> def
                    in prepend s (findcs (csm . (prepend s)))
                in csmx (findcs csmx)
            Nothing -> Nothing

-- | There are a finite number of values (possibly zero).
class (Searchable a, Countable a) => Finite a where
    -- | Not necessarily in counting order.
    allValues :: [a]
    assemble ::
           forall b f. (Applicative f)
        => (a -> f b)
        -> f (a -> b)
    assemble afb = fmap listLookup (traverse (\a -> fmap (\b -> (a, b)) (afb a)) allValues)
      where
        -- listLookup :: [(a,b)] -> a -> b;
        listLookup [] _ = error "missing value" -- this should never happen
        listLookup ((a, b):_) a'
            | a == a' = b
        listLookup (_:l) a' = listLookup l a'

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust ((Just a):_) = Just a
firstJust (Nothing:mas) = firstJust mas

finiteSearch :: (Finite a) => (a -> Maybe b) -> Maybe b
finiteSearch p = firstJust (fmap p allValues)

finiteCountPrevious :: (Finite a) => a -> Maybe a
finiteCountPrevious x = findp Nothing allValues
  where
    findp ma (a:_)
        | a == x = ma
    findp _ (a:as) = findp (Just a) as
    findp _ [] = seq x (error "missing value")

firstItem :: [a] -> Maybe a
firstItem [] = Nothing
firstItem (a:_) = Just a

finiteCountMaybeNext :: (Finite a) => Maybe a -> Maybe a
finiteCountMaybeNext Nothing = firstItem allValues
finiteCountMaybeNext (Just x) = findmn allValues
  where
    findmn (a:as)
        | x == a = firstItem as
    findmn (_:as) = findmn as
    findmn [] = seq x (error "missing value")

instance Searchable Void where
    search = finiteSearch

instance Finite Void where
    allValues = []
    assemble _ = pure absurd

instance Searchable () where
    search = finiteSearch

instance Finite () where
    allValues = [()]
    assemble afb = liftA (\v _ -> v) (afb ())

instance Searchable Bool where
    search = finiteSearch

instance Finite Bool where
    allValues = [False, True]
    assemble afb =
        liftA2
            (\f t x ->
                 if x
                     then t
                     else f)
            (afb False)
            (afb True)

instance Searchable Word8 where
    search = finiteSearch

instance Finite Word8 where
    allValues = enumFrom minBound

instance Searchable Word16 where
    search = finiteSearch

instance Finite Word16 where
    allValues = enumFrom minBound

instance Searchable Word32 where
    search = finiteSearch

instance Finite Word32 where
    allValues = enumFrom minBound

instance Searchable Word64 where
    search = finiteSearch

instance Finite Word64 where
    allValues = enumFrom minBound

instance Searchable Int8 where
    search = finiteSearch

instance Finite Int8 where
    allValues = enumFrom minBound

instance Searchable Int16 where
    search = finiteSearch

instance Finite Int16 where
    allValues = enumFrom minBound

instance Searchable Int32 where
    search = finiteSearch

instance Finite Int32 where
    allValues = enumFrom minBound

instance Searchable Int64 where
    search = finiteSearch

instance Finite Int64 where
    allValues = enumFrom minBound

instance (Finite a) => Finite (Maybe a) where
    allValues = Nothing : (fmap Just allValues)
    assemble mafb = liftA2 maybe (mafb Nothing) (assemble (mafb . Just))

instance (Finite a, Finite b) => Finite (Either a b) where
    allValues = (fmap Left allValues) ++ (fmap Right allValues)
    assemble eabfr = liftA2 either (assemble (eabfr . Left)) (assemble (eabfr . Right))

instance (Finite a, Finite b) => Finite (a, b) where
    allValues = liftA2 (,) allValues allValues
    assemble abfr = fmap (\abr (a, b) -> abr a b) (assemble (\a -> assemble (\b -> abfr (a, b))))
