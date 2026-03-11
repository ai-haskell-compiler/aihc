{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-|

An API implementing a convenient syntax for defining maps.  This module was
born from the observation that a list of tuples is semantically ambiguous
about how duplicate keys should be handled.  Additionally, the syntax is
inherently rather cumbersome and difficult to work with.  This API takes
advantage of do notation to provide a very light syntax for defining maps
while at the same time eliminating the semantic ambiguity of alists.

Here's an example:

> foo :: MapSyntax Text Text
> foo = do
>   "firstName" ## "John"
>   "lastName"  ## "Smith"

-}

module Data.Map.Syntax
  (
  -- * Core API
    MapSyntaxM
  , MapSyntax
  , runMap
  , (##)
  , (#!)
  , (#?)
  , mapK
  , mapV
  , runMapSyntax
  , runMapSyntax'

  -- * Lower level functions
  , DupStrat(..)
  , ItemRep(..)
  , addStrat
  ) where


------------------------------------------------------------------------------
import           Control.Monad.State
import qualified Data.Map            as M

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Strategy to use for duplicates
data DupStrat = Replace | Ignore | Error

{-

Note: We don't use this seemingly more general formulation:

type DupStrat k v = k -> v -> v -> Either k v

...because it is contravariant in k and v and makes it impossible to implement
mapK and mapV.

-}

------------------------------------------------------------------------------
-- | Representation of an indivdual item in a map
data ItemRep k v = ItemRep
    { irStrat :: DupStrat
    , irKey :: k
    , irVal :: v
    }


------------------------------------------------------------------------------
type MapRep k v = [ItemRep k v] -> [ItemRep k v]


------------------------------------------------------------------------------
-- | A monad providing convenient syntax for defining maps.
newtype MapSyntaxM k v a = MapSyntaxM { unMapSyntax :: State (MapRep k v) a }
  deriving (Functor, Applicative, Monad)


------------------------------------------------------------------------------

instance Semigroup (MapSyntax k v) where
  (<>) = (>>)

instance Monoid (MapSyntax k v) where
  mempty = pure $! ()
#if !MIN_VERSION_base(4,11,0)
  mappend = (<>)
#endif

------------------------------------------------------------------------------
-- | Convenient type alias that will probably be used most of the time.
type MapSyntax k v = MapSyntaxM k v ()


------------------------------------------------------------------------------
-- | Low level add function for adding a specific DupStrat, key, and value.
addStrat :: DupStrat -> k -> v -> MapSyntax k v
addStrat strat k v = addStrat' [ItemRep strat k v]


------------------------------------------------------------------------------
addStrat' :: [ItemRep k v] -> MapSyntax k v
addStrat' irs = MapSyntaxM $ modify (\ir -> ir . (irs ++))


------------------------------------------------------------------------------
-- | Forces an entry to be added.  If the key already exists, its value is
-- overwritten.
(##) :: k -> v -> MapSyntax k v
(##) = addStrat Replace
infixr 0 ##


------------------------------------------------------------------------------
-- | Tries to add an entry, but if the key already exists, then 'runMap' will
-- return a Left with the list of offending keys.  This may be useful if name
-- collisions are bad and you want to know when they occur.
(#!) :: k -> v -> MapSyntax k v
(#!) = addStrat Error
infixr 0 #!


------------------------------------------------------------------------------
-- | Inserts into the map only if the key does not already exist.  If the key
-- does exist, it silently continues without overwriting or generating an
-- error indication.
(#?) :: k -> v -> MapSyntax k v
(#?) = addStrat Ignore
infixr 0 #?


------------------------------------------------------------------------------
-- | Runs the MapSyntaxM monad to generate a map.
runMap :: Ord k => MapSyntaxM k v a -> Either [k] (M.Map k v)
runMap = runMapSyntax M.lookup M.insert


------------------------------------------------------------------------------
-- | Runs the MapSyntaxM monad to generate a map.
runMapSyntax
    :: (Monoid map)
    => (k -> map -> Maybe v)
    -- ^ Function that gets a key's value
    -> (k -> v -> map -> map)
    -- ^ Function to force-insert a key-value pair into the map
    -> MapSyntaxM k v a
    -> Either [k] map
runMapSyntax = runMapSyntax' (\_ _ _ -> Nothing)


------------------------------------------------------------------------------
-- | Runs the MapSyntaxM monad to generate a map.  This function gives you the
-- full power of insertWith when duplicate keys are encountered.
--
-- Example:
--
-- > runMapSyntax' (\k new_val old_val -> Just $ old_val ++ new_val)
runMapSyntax'
    :: (Monoid map)
    => (k -> v -> v -> Maybe v)
    -- ^ Function to handle duplicate key insertion, similar to the first
    -- argument to insertWith.  If this function returns Nothing, then this is
    -- interpreted as an error.  If it is a Just, then the resulting value
    -- will be inserted into the map.
    -> (k -> map -> Maybe v)
    -- ^ Function that gets a key's value
    -> (k -> v -> map -> map)
    -- ^ Function to force-insert a key-value pair into the map
    -> MapSyntaxM k v a
    -> Either [k] map
runMapSyntax' dupFunc lookupEntry forceIns ms =
    case res of
      ([],m) -> Right m
      (es,_) -> Left es
  where
    res = foldl f (mempty, mempty) $ execState (unMapSyntax ms) id []
    f accum@(es,m) ir@ItemRep{..} =
      case lookupEntry irKey m of
        Just v1 -> replace accum ir v1
        Nothing -> (es, forceIns irKey irVal m)

    replace (es,m) ir v1 =
      case irStrat ir of
        Replace -> (es, forceIns (irKey ir) (irVal ir) m)
        Ignore -> (es, m)
        Error -> maybe (es ++ [irKey ir], m)
                       (\v -> (es, forceIns (irKey ir) v m)) $
                       dupFunc (irKey ir) (irVal ir) v1


------------------------------------------------------------------------------
execMapSyntax :: MapSyntaxM k v a -> MapRep k v
execMapSyntax ms = execState (unMapSyntax ms) id


------------------------------------------------------------------------------
-- | Maps a function over all the keys.
mapK :: (k1 -> k2) -> MapSyntaxM k1 v a -> MapSyntax k2 v
mapK f ms = addStrat' items
  where
    items = map (\ir -> ir { irKey = f (irKey ir) }) $ execMapSyntax ms []


------------------------------------------------------------------------------
-- | Maps a function over all the values.
mapV :: (v1 -> v2) -> MapSyntaxM k v1 a -> MapSyntax k v2
mapV f ms = addStrat' items
  where
    items = map (\ir -> ir { irVal = f (irVal ir ) }) $ execMapSyntax ms []
