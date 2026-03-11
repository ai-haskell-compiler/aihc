module Data.Renderable (
    RenderStrategy(..),
    Renderer,
    Rendering,
    CleanOp,
    Cache,
    CacheStats(..),
    renderPrims,
    renderPrimsDebug,
    renderPrimsWithStats,
    emptyRenderer,
    appendRenderer
) where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Hashable
import qualified Data.Traversable as T
import Data.IntMap (IntMap)
import Data.Foldable (foldl')
import qualified Data.IntMap as IM
--------------------------------------------------------------------------------
-- A strategy for rendering
--------------------------------------------------------------------------------
-- | A 'RenderStrategy' is a method for creating a renderer that can render
-- your primitives. Examples of primitives are are points, lines, triangles and
-- other shapes. A 'RenderStrategy' is parameterized by four types -
--
-- @m@ - the monad in which rendering calls will take place.
--
-- @t@ - type of the graphics transformation that can be applied to the
--       renderer
--
-- @r@ - type that holds static resources such as windows, shaders, etc.
--
-- @a@ - type of the primitive that can be renderered.
data RenderStrategy m t r a = RenderStrategy
    { canAllocPrimitive :: r -> a -> Bool
      -- ^ Determines whether a renderer can be allocated for the primitive.
      -- A result of 'False' will defer compilation until a later time (the next
      -- frame).

    , compilePrimitive :: r -> a -> m (Renderer m t)
      -- ^ Allocates resources for rendering the primitive and return
      -- a monadic call that renders the primitive using a transform.
      -- Tuples that with a call to clean up the allocated resources.
    }
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
-- | A Rendering is an effectful computation for displaying something given a
-- transform.
type Rendering m t = t -> m ()

-- | A CleanOp is an effectfull computaton that cleans up any resources
-- allocated during the creation of an associated Rendering.
type CleanOp m = m ()

-- | A Renderer is the pairing of a Rendering and a Cleanup.
type Renderer m t = (CleanOp m, Rendering m t)

-- | Create a renderer that renders nothing and releases no resources.
emptyRenderer :: Monad m => Renderer m t
emptyRenderer = (return (), const $ return ())

-- | Appends two renderers into one.
appendRenderer :: Monad m => Renderer m t -> Renderer m t -> Renderer m t
appendRenderer (c1,r1) (c2,r2) = (c1 >> c2, \t -> r1 t >> r2 t)

-- | A cache of renderers.
type Cache m t = IntMap (Renderer m t)

findRenderer :: (Monad m, Hashable a)
             => Cache m t -> (Cache m t, IntMap a) -> a -> (Cache m t, IntMap a)
findRenderer cache (found, missing) a =
    let k = hash a in
    case IM.lookup k cache of
        Nothing -> (found, IM.insert k a missing)
        Just r  -> (IM.insert k r found, missing)

getRenderer :: (Hashable a, Monad m)
            => RenderStrategy m t r a -> r -> Cache m t -> a -> m (Cache m t)
getRenderer s rez cache a =
    if canAllocPrimitive s rez a
    then do r <- compilePrimitive s rez a
            return $ IM.insert (hash a) r cache
    else return cache

clean :: Renderer m t -> m ()
clean = fst

render :: Renderer m t -> t -> m ()
render = snd

renderElement :: (Hashable a, Monad m) => Cache m t -> t -> a -> m ()
renderElement cache t a = do
    let k = hash a
    case IM.lookup k cache of
        Nothing -> return ()
        Just r  -> render r t

-- | A sum of lists of rendering hashes between two cache states.
-- Used for debugging resource management.
data CacheStats a = CacheStats { cachedPrev    :: [Int]
                               -- ^ All the keys of the previous cache state.
                               , cachedFound   :: [Int]
                               -- ^ The keys needed for the next state that
                               -- were found in the previous cache (no need
                               -- to allocate).
                               , cachedMissing :: [Int]
                               -- ^ The keys needed for the next state that
                               -- were not found in the previous cache (these
                               -- will need allocating).
                               , cachedStale   :: [Int]
                               -- ^ The keys found in the previous cache that
                               -- are not needed for the next state (these
                               -- can be deallocated).
                               , cachedNext    :: [Int]
                               -- ^ All the keys of the next cache state.
                               }

-- | Map a 'CacheStats' into a nice readable string.
showCacheStats :: CacheStats a -> String
showCacheStats (CacheStats cache found missing stale next) = unlines
    [ "Prev:    " ++ show cache
    , "Found:   " ++ show found
    , "Missing: " ++ show missing
    , "Stale:   " ++ show stale
    , "Next:    " ++ show next
    ]

-- | Render a list of primitives using renderings stored in the given cache,
-- return a new cache that can be used to render the next list of
-- primitives, along with some info about the comparison of the given and
-- returned cache.
renderPrimsWithStats :: (Functor m, Monad m, Monoid t, Hashable a)
                     => RenderStrategy m t r a -> r -> Cache m t -> [(t, a)]
                     -> m (Cache m t, CacheStats a)
renderPrimsWithStats s rez cache prims = do
    let (found, missing) = foldl' (findRenderer cache)
                                  (mempty, mempty)
                                  (map snd prims)
        stale = cache `IM.difference` found

    -- Clean the stale renderers
    void $ T.sequence $ fmap clean stale

    -- Get the missing renderers
    new <- foldM (getRenderer s rez) mempty $ IM.elems missing

    let next = IM.union found new
        stats = CacheStats { cachedPrev = IM.keys cache
                           , cachedFound = IM.keys found
                           , cachedMissing = IM.keys missing
                           , cachedStale = IM.keys stale
                           , cachedNext = IM.keys next
                           }

    -- Render the composite
    mapM_ (uncurry $ renderElement next) prims
    return (next,stats)

-- | Render a list of primitives using renderings stored in the given cache,
-- return a new cache that can be used to render the next list of
-- primitives. Optionally print some debug info.
renderPrimsDebug :: (Functor m, MonadIO m, Monoid t, Hashable a)
                 => Bool -> RenderStrategy m t r a -> r -> Cache m t -> [(t, a)]
                 -> m (Cache m t)
renderPrimsDebug debug s rez cache prims = do
    (next, stats) <- renderPrimsWithStats s rez cache prims
    when debug $ liftIO $ putStrLn $ showCacheStats stats
    return next

-- | Render a list of primitives using renderings stored in the given cache,
-- return a new cache that can be used to render the next list of
-- primitives.
renderPrims :: (Functor m, Monad m, Monoid t, Hashable a)
            => RenderStrategy m t r a -> r -> Cache m t -> [(t, a)]
            -> m (Cache m t)
renderPrims s rez cache prims = fst <$> renderPrimsWithStats s rez cache prims
