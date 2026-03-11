{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK not-home #-}
#if __GLASGOW_HASKELL__ >= 904
-- See Note [-fdmd-unbox-width]
{-# OPTIONS_GHC -fdmd-unbox-width=4 #-}
#endif

-- | This is an internal module. You probably don't need to import this.
--
-- = WARNING
--
-- Definitions in this module allow violating invariants that would otherwise be
-- guaranteed by non-internal modules. Use at your own risk!
--
module Regex.Internal.Parser
  ( Parser(..)
  , Node(..)
  , compile
  , compileBounded

  , ParserState
  , prepareParser
  , stepParser
  , finishParser
  , Foldr
  , parseFoldr
  , parseNext
  ) where

import Control.Applicative ((<|>), empty)
import qualified Control.Applicative as Ap
import Control.Monad.Trans.State.Strict
  ( State, StateT, evalState, evalStateT, gets, modify', state)
import Control.Monad.Fix (mfix)
import Data.Maybe (isJust)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
#ifdef __GLASGOW_HASKELL__
import Data.Primitive.SmallArray
  (SmallArray, emptySmallArray, smallArrayFromList)
import qualified GHC.Exts as X
#endif

import Regex.Internal.Regex (RE(..))
import Regex.Internal.Solo (Solo, matchSolo)
import Regex.Internal.Unique (Unique(..), UniqueSet)
import qualified Regex.Internal.Unique as U

----------
-- Types
----------

-- | A parser compiled from a @'RE' c a@.
data Parser c a where
  PToken  :: !(c -> Maybe a) -> Parser c a
  PFmap   :: !(a1 -> Solo a) -> !(Parser c a1) -> Parser c a
  PFmap_  :: !(Node c a) -> Parser c a
  PPure   :: a -> Parser c a
  PLiftA2 :: !(a1 -> a2 -> Solo a) -> !(Parser c a1) -> !(Parser c a2) -> Parser c a
  PEmpty  :: Parser c a
  PAlt    :: {-# UNPACK #-} !Unique -> !(Parser c a) -> !(Parser c a) -> {-# UNPACK #-} !(SmallArray (Parser c a)) -> Parser c a
  PFoldGr :: {-# UNPACK #-} !Unique -> !(a -> a1 -> Solo a) -> a -> !(Parser c a1) -> Parser c a
  PFoldMn :: {-# UNPACK #-} !Unique -> !(a -> a1 -> Solo a) -> a -> !(Parser c a1) -> Parser c a
  PMany   :: {-# UNPACK #-} !Unique -> !(a1 -> Solo a) -> !(a2 -> Solo a) -> !(a2 -> a1 -> Solo a2) -> !a2 -> !(Parser c a1) -> Parser c a

-- | A node in the NFA. Used for recognition.
data Node c a where
  NAccept :: a -> Node c a
  NGuard  :: {-# UNPACK #-} !Unique -> Node c a -> Node c a
  NToken  :: !(c -> Maybe a1) -> !(Node c a) -> Node c a
  NEmpty  :: Node c a
  NAlt    :: !(Node c a) -> !(Node c a) -> {-# UNPACK #-} !(SmallArray (Node c a)) -> Node c a
-- Note that NGuard is lazy in the node. We have to introduce laziness in
-- at least one place, to make a graph with loops possible.

------------
-- Compile
------------

-- | \(O(m)\). Compile a @RE c a@ to a @Parser c a@.
--
-- Note: @compile@ does not limit the size of the @RE@. See 'compileBounded'
-- if you would like to limit the size.
-- @RE@s with size greater than @(maxBound::Int) \`div\` 2@ are not supported
-- and the behavior of such a @RE@ is undefined.
compile :: RE c a -> Parser c a
compile re = evalState (compileToParser re) (Unique 0)

nxtU :: State Unique Unique
nxtU = state $ \u -> let !u' = Unique (unUnique u + 1) in (u, u')

compileToParser :: RE c a -> State Unique (Parser c a)
compileToParser re = case re of
  RToken t -> pure $ PToken t
  RFmap f re1 -> PFmap f <$> compileToParser re1
  RFmap_ a re1 -> PFmap_ <$> compileToNode a re1
  RPure a -> pure $ PPure a
  RLiftA2 f re1 re2 ->
    Ap.liftA2 (PLiftA2 f) (compileToParser re1) (compileToParser re2)
  REmpty -> pure PEmpty
  RAlt re01 re02 -> do
    u <- nxtU
    let (re1,re2,res) = gatherAlts re01 re02
    p1 <- compileToParser re1
    p2 <- compileToParser re2
    ps <- T.traverse compileToParser res
    pure $ PAlt u p1 p2 (smallArrayFromList ps)
  RFoldGr f z re1 -> do
    u <- nxtU
    _localU <- nxtU
    PFoldGr u f z <$> compileToParser re1
  RFoldMn f z re1 -> do
    u <- nxtU
    _localU <- nxtU
    PFoldMn u f z <$> compileToParser re1
  RMany f1 f2 f z re1 -> do
    u <- nxtU
    _localU <- nxtU
    PMany u f1 f2 f z <$> compileToParser re1

compileToNode :: forall c a a1. a -> RE c a1 -> State Unique (Node c a)
compileToNode a re0 = go re0 (NAccept a)
  where
    go :: forall a2. RE c a2 -> Node c a -> State Unique (Node c a)
    go re nxt = case re of
      RToken t -> pure $ NToken t nxt
      RFmap _ re1 -> go re1 nxt
      RFmap_ _ re1 -> go re1 nxt
      RPure _ -> pure nxt
      RLiftA2 _ re1 re2 -> go re2 nxt >>= go re1
      REmpty -> pure NEmpty
      RAlt re01 re02 -> do
        u <- nxtU
        let nxt1 = NGuard u nxt
            (re1,re2,res) = gatherAlts re01 re02
        n1 <- go re1 nxt1
        n2 <- go re2 nxt1
        ns <- T.traverse (flip go nxt1) res
        pure $ NAlt n1 n2 (smallArrayFromList ns)
      RFoldGr _ _ re1 -> goMany True re1 nxt
      RFoldMn _ _ re1 -> goMany False re1 nxt
      RMany _ _ _ _ re1 -> goMany True re1 nxt
    goMany :: forall a2. Bool -> RE c a2 -> Node c a -> State Unique (Node c a)
    goMany greedy re1 nxt = do
      u <- nxtU
      mfix $ \n -> do
        ndown <- go re1 n
        if greedy
        then pure $ NGuard u (NAlt ndown nxt emptySmallArray)
        else pure $ NGuard u (NAlt nxt ndown emptySmallArray)

gatherAlts :: RE c a -> RE c a -> (RE c a, RE c a, [RE c a])
gatherAlts re01 re02 = case go re01 (go re02 []) of
  re11:re12:res -> (re11, re12, res)
  _ -> error "Regex.Internal.Parser.gatherAlts: impossible"
  where
    go (RAlt re1 re2) acc = go re1 (go re2 acc)
    go re acc = re:acc

--------------------
-- Compile bounded
--------------------

-- | \(O(\min(l,m))\). Compile a @RE c a@ to a @Parser c a@.
--
-- Returns @Nothing@ if the size of the @RE@ is greater than the provided limit
-- \(l\). You may want to use this if you suspect that the @RE@ may be too
-- large, for instance if the regex is constructed from an untrusted source.
--
-- While the exact size of a @RE@ depends on an internal representation, it can
-- be assumed to be in the same order as the length of a
-- [regex pattern](https://en.wikipedia.org/wiki/Regular_expression#Syntax)
-- corresponding to the @RE@.
compileBounded :: Int -> RE c a -> Maybe (Parser c a)
compileBounded lim re =
  if checkSize lim re
  then Just $! compile re
  else Nothing

checkSize :: Int -> RE c a -> Bool
checkSize lim re0 = isJust (evalStateT (go re0) 0)
  where
    go :: RE c a1 -> StateT Int Maybe ()
    go re = case re of
        RToken _ -> inc
        RFmap _ re1 -> inc *> go re1
        RFmap_ _ re1 -> inc *> go re1
        RPure _ -> inc
        RLiftA2 _ re1 re2 -> inc *> go re1 *> go re2
        REmpty -> inc
        RAlt re1 re2 -> inc *> go re1 *> go re2
        RFoldGr _ _ re1 -> inc *> go re1
        RFoldMn _ _ re1 -> inc *> go re1
        RMany _ _ _ _ re1 -> inc *> go re1
    inc = do
      ok <- gets (< lim)
      if ok
      then modify' (+1)
      else empty

----------
-- Parse
----------

data Cont c b a where
  CTop     :: Cont c a a
  CFmap    :: !(b -> Solo a1) -> !(Cont c a1 a) -> Cont c b a
  CFmap_   :: !(Node c a1) -> !(Cont c a1 a) -> Cont c b a
  CLiftA2A :: !(b -> a2 -> Solo a3) -> !(Parser c a2) -> !(Cont c a3 a) -> Cont c b a
  CLiftA2B :: !(a1 -> b -> Solo a3) -> a1 -> !(Cont c a3 a) -> Cont c b a
  CAlt     :: {-# UNPACK #-} !Unique -> !(Cont c b a) -> Cont c b a
  CFoldGr  :: {-# UNPACK #-} !Unique -> !(Parser c b) -> !(a1 -> b -> Solo a1) -> a1 -> !(Cont c a1 a) -> Cont c b a
  CFoldMn  :: {-# UNPACK #-} !Unique -> !(Parser c b) -> !(a1 -> b -> Solo a1) -> a1 -> !(Cont c a1 a) -> Cont c b a
  CMany    :: {-# UNPACK #-} !Unique -> !(Parser c b) -> !(b -> Solo a2) -> !(a1 -> Solo a2) -> !(a1 -> b -> Solo a1) -> !a1 -> !(Cont c a2 a) -> Cont c b a

data NeedCList c a where
  NeedCCons :: !(c -> Maybe b) -> !(Cont c b a) -> !(NeedCList c a) -> NeedCList c a
  NeedCNil :: NeedCList c a

data StepState c a = StepState
  { sSet :: {-# UNPACK #-} !UniqueSet
  , sNeed :: !(NeedCList c a)
  , sResult :: !(Maybe a)
  }

stepStateZero :: StepState c a
stepStateZero = StepState U.empty NeedCNil Nothing

sMember :: Unique -> StepState c a -> Bool
sMember u pt = U.member u (sSet pt)

sInsert :: Unique -> StepState c a -> StepState c a
sInsert u pt = pt { sSet = U.insert u (sSet pt) }

-- Note [-fdmd-unbox-width]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- GHC's worker/wrapper transformation is able to eliminate the StepState and
-- generate a worker for `down` with signature
--
-- $wdown
--   :: Parser c b -> Cont c b a
--   -> Int# -> IntSet -> NeedCList c a -> Maybe a
--   -> (# Int#, IntSet, NeedCList c a, Maybe a #)
--
-- and likewise for `downNode` and `up`.
--
-- This is great, but unfortunately boxity analysis gets in the way. Boxity
-- analysis prevents unboxing of types with more than -fdmd-unbox-width fields,
-- default 3 as of today. So we set it to the number of fields in StepState,
-- i.e. 4, with an OPTIONS_GHC pragma.

down :: Parser c b -> Cont c b a -> StepState c a -> StepState c a
down p !ct !pt = case p of
  PToken t -> pt { sNeed = NeedCCons t ct (sNeed pt) }
  PFmap f p1 -> down p1 (CFmap f ct) pt
  PFmap_ n -> downNode n ct pt
  PPure b -> up b ct pt
  PLiftA2 f p1 p2 -> down p1 (CLiftA2A f p2 ct) pt
  PEmpty -> pt
  PAlt u p1 p2 ps ->
    let ct1 = CAlt u ct
    in F.foldl' (\pt' p' -> down p' ct1 pt') (down p2 ct1 (down p1 ct1 pt)) ps
  PFoldGr u f z p1 ->
    if sMember u pt
    then pt
    else
      let pt1 = down p1 (CFoldGr u p1 f z ct) (sInsert (localU u) pt)
      in if sMember u pt1
         then pt1
         else up z ct (sInsert u pt1)
  PFoldMn u f z p1 ->
    if sMember u pt
    then pt
    else
      let pt1 = if sMember (localU u) pt
                then pt
                else up z ct pt
      in down p1 (CFoldMn u p1 f z ct) (sInsert u pt1)
  PMany u f1 f2 f z p1 ->
    if sMember u pt
    then pt
    else
      let pt1 = down p1 (CMany u p1 f1 f2 f z ct) (sInsert (localU u) pt)
      in if sMember u pt1
         then pt1
         else matchSolo (f2 z) $ \x -> up x ct (sInsert u pt1)

downNode :: Node c b -> Cont c b a -> StepState c a -> StepState c a
downNode n !ct !pt = case n of
  NAccept b -> up b ct pt
  NGuard u n1 ->
    if sMember u pt
    then pt
    else downNode n1 ct (sInsert u pt)
  NToken t nxt ->
    pt { sNeed = NeedCCons t (CFmap_ nxt ct) (sNeed pt) }
  NEmpty -> pt
  NAlt n1 n2 ns ->
    F.foldl'
      (\pt' n' -> downNode n' ct pt')
      (downNode n2 ct (downNode n1 ct pt))
      ns

up :: b -> Cont c b a -> StepState c a -> StepState c a
up b ct !pt = case ct of
  CTop -> pt { sResult = sResult pt <|> Just b }
  CFmap f ct1 -> matchSolo (f b) $ \x -> up x ct1 pt
  CFmap_ n ct1 -> downNode n ct1 pt
  CLiftA2A f p1 ct1 -> down p1 (CLiftA2B f b ct1) pt
  CLiftA2B f a ct1 -> matchSolo (f a b) $ \x -> up x ct1 pt
  CAlt u ct1 ->
    if sMember u pt
    then pt
    else up b ct1 (sInsert u pt)
  CFoldGr u p1 f z ct1 ->
    if sMember u pt
    then pt
    else
      if sMember (localU u) pt
      then up z ct1 (sInsert u pt)
      else matchSolo (f z b) $ \z1 ->
        let pt1 = down p1 (CFoldGr u p1 f z1 ct1) pt
        in up z1 ct1 (sInsert u pt1)
  CFoldMn u p1 f z ct1 ->
    if sMember u pt
    then pt
    else matchSolo (f z b) $ \z1 ->
      let pt1 = up z1 ct1 (sInsert (localU u) pt)
      in if sMember u pt1
         then pt1
         else down p1 (CFoldMn u p1 f z1 ct1) (sInsert u pt1)
  CMany u p1 f1 f2 f z ct1 ->
    if sMember u pt
    then pt
    else
      if sMember (localU u) pt
      then matchSolo (f1 b) $ \x -> up x ct1 (sInsert u pt)
      else
        matchSolo (f z b) $ \z1 ->
        matchSolo (f2 z1) $ \x ->
          let pt1 = down p1 (CMany u p1 f1 f2 f z1 ct1) pt
          in up x ct1 (sInsert u pt1)

localU :: Unique -> Unique
localU = Unique . (+1) . unUnique

--------------------
-- Running a Parser
--------------------

-- | The state maintained for parsing.
data ParserState c a = ParserState
  { psNeed :: !(NeedCList c a)
  , psResult :: !(Maybe a)
  }

-- | \(O(m \log m)\). Prepare a parser for input.
--
-- Returns @Nothing@ if parsing has failed regardless of further input.
-- Otherwise, returns the initial @ParserState@.
prepareParser :: Parser c a -> Maybe (ParserState c a)
prepareParser p = toParserState (down p CTop stepStateZero)

-- | \(O(m \log m)\). Step a parser by feeding a single element @c@.
--
-- Returns @Nothing@ if parsing has failed regardless of further input.
-- Otherwise, returns an updated @ParserState@.
stepParser :: ParserState c a -> c -> Maybe (ParserState c a)
stepParser ps c0 = case psNeed ps of
  NeedCNil -> Nothing
  needs -> toParserState (go c0 needs)
  where
    go c (NeedCCons t ct rest) =
      let !pt = go c rest
      in maybe pt (\b -> up b ct pt) (t c)
    go _ NeedCNil = stepStateZero
{-# INLINE stepParser #-}

-- | \(O(1)\). Get the parse result for the input fed into the parser so far.
finishParser :: ParserState c a -> Maybe a
finishParser = psResult

toParserState :: StepState c a -> Maybe (ParserState c a)
toParserState ss = case (sNeed ss, sResult ss) of
  (NeedCNil, Nothing) -> Nothing
  (need, result) -> Just $! ParserState { psNeed = need, psResult = result }

-- | A fold function.
type Foldr f a = forall b. (a -> b -> b) -> b -> f -> b

-- | \(O(mn \log m)\). Run a parser given a sequence @f@ and a fold function.
--
-- Parses the entire sequence, not just a prefix or an substring.
-- Returns early on parse failure, if the fold can short circuit.
--
-- ==== __Examples__
--
-- @
-- import qualified Regex.Base as R
-- import qualified Data.Vector.Generic as VG -- from vector
--
-- parseVector :: VG.Vector v c => R.Parser c a -> v c -> Maybe a
-- parseVector p v = R.'parseFoldr' VG.foldr p v
-- @
--
-- >>> import Control.Applicative (many)
-- >>> import qualified Regex.Base as R
-- >>> import qualified Data.Vector as V
-- >>>
-- >>> let p = R.compile $ many ((,) <$> R.satisfy even <*> R.satisfy odd) :: Parser Int [(Int, Int)]
-- >>> parseVector p (V.fromList [6,1,2,5,4,3])
-- Just [(6,1),(2,5),(4,3)]
-- >>> parseVector p (V.fromList [4,3,1,2])
-- Nothing
--
parseFoldr :: Foldr f c -> Parser c a -> f -> Maybe a
parseFoldr fr = \p xs -> prepareParser p >>= fr f finishParser xs
  where
    f c k =
#ifdef __GLASGOW_HASKELL__
      X.oneShot
#endif
        (\ !ps -> stepParser ps c >>= k)
{-# INLINE parseFoldr #-}

-- | \(O(mn \log m)\). Run a parser given a \"@next@\" action.
--
-- Calls @next@ repeatedly to yield elements. A @Nothing@ is interpreted as
-- end-of-sequence.
--
-- Parses the entire sequence, not just a prefix or an substring.
-- Returns without exhausting the input on parse failure.
--
-- ==== __Examples__
--
-- @
-- import qualified Regex.Base as R
-- import qualified Conduit as C -- from conduit
--
-- parseConduit :: Monad m => R.Parser c a -> C.ConduitT c x m (Maybe a)
-- parseConduit p = R.'parseNext' p C.await <* C.sinkNull
-- @
--
-- >>> import Control.Applicative (many)
-- >>> import qualified Regex.Base as R
-- >>> import Conduit ((.|))
-- >>> import qualified Conduit as C
-- >>>
-- >>> let p = R.compile $ many ((,) <$> R.satisfy even <*> R.satisfy odd) :: Parser Int [(Int, Int)]
-- >>> runConduit $ C.yieldMany [0..3] .| C.iterMC print .| parseConduit p
-- 0
-- 1
-- 2
-- 3
-- Just [(0,1),(2,3)]
-- >>> runConduit $ C.yieldMany [4,3,1,2] .| C.iterMC print .| parseConduit p
-- 4
-- 3
-- 1
-- 2
-- Nothing
--
-- @since 0.2.0.0
parseNext :: Monad m => Parser c a -> m (Maybe c) -> m (Maybe a)
parseNext p next = case prepareParser p of
  Nothing -> pure Nothing
  Just ps -> loop ps
  where
    loop !ps = next >>= \m -> case m of
      Nothing -> pure (finishParser ps)
      Just c -> case stepParser ps c of
        Nothing -> pure Nothing
        Just ps' -> loop ps'
{-# INLINE parseNext #-}

-----------------
-- Array compat
-----------------

#ifndef __GLASGOW_HASKELL__
type SmallArray = []

emptySmallArray :: SmallArray a
emptySmallArray = []

smallArrayFromList :: [a] -> SmallArray a
smallArrayFromList = id
#endif

----------
-- Notes
----------

-- Note [About the algorithm]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- To parse using a regex, we compile the regex into a non-deterministic finite
-- automata (NFA). Actually, we only only do this for recognition, i.e. checking
-- whether a sequence satisfies a regex. This is done if the regex is a RFmap_.
--
-- To parse into a value, we have to do more work. We keep the regex as a tree
-- (Parser), but we preserve the path taken down the tree, like a zipper.
-- This lets us go up the tree and continue parsing, once we parse a c.
-- If you squint your eyes, this is also an NFA, only each edge of the NFA is
-- broken into multiple steps up and down the tree.
--
-- Recognition using the NFA is faster than parsing, unsurprisingly.
-- A Parser tree can have NFAs as children. This means that if some subtree of
-- the regex only attempts to recognize some input, it doesn't pay the extra
-- cost of parsing.
--
-- Key objective: O(mn log m) time parsing. This means that for every c fed into
-- the parser, we are allowed to take no more than O(m log m) time.
--
-- How is this ensured?
-- 1. The compiled regex must have O(m) nodes and O(m) edges. The Parser tree
--    satisfies this, of course, since it reflects the regex itself. The NFA
--    also satisfies this, implemented as Thompson's construction:
--    https://en.wikipedia.org/wiki/Thompson%27s_construction
-- 2. For every c, no edge is traversed twice. Tree edges are bidirectional
--    unlike NFA edges, so an NFA edge may be traversed only once and a tree
--    edge may be traversed once in each direction.
--
-- NFA guards: To ensure each NFA edge can be traversed only once, guard nodes
-- (NGuard) carry a Unique which can be stored in a set (sSet). Guard nodes are
-- created during compilation whenever two nodes would lead into one node:
-- A->C, B->C. A guard node is added, such that it becomes A->G, B->G, G->C.
--
-- Parser guards: Parser guards are more tricky.
-- Alt: There are two ways into an Alt node when going up. So, an Alt node
--   carries a Unique is stored in sSet and guards upward travel through the
--   node.
-- FoldGr: There are two ways into a FoldGr node, one going down and one going
--   up. But, we can't just a use a Unique to guard entry into it because we
--   want to handle loopy cases correctly! A loopy case is where we reach the
--   same node in the tree by going up and down the edges without consuming
--   input. To detect this, we use a separate Unique (localU) when going down.
--   If we find it set when going up, we are looping. When we send up a value,
--   looping or not, we guard entry into the node using its (not localU) Unique.
-- Many: A Many node is just like FoldlGr, only the looping case is handled
--   specially.
-- FoldMn: Like FoldGr, there are two ways into a FoldlMn node, one going down
--   and one going up, and we must handle loopy cases correctly. A FoldMn sends
--   a value up before going down. So, the localU is set when going up and if
--   we find it when going down, we are looping. When we send down a value, we
--   guard entry into the node using its (not localU) Unique.

-- Note [Regex optimizations]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Currently, the only optimization performed is
--
-- * Gather multiple RAlts into a single multi-way branching PAlt/NAlt. It's
--   better to multi-way branch at a flat array instead of nested 2-way
--   branches, much less pointer-chasing.
--
-- Other possible optimizations are possible when compiling, such as removing
-- paths going to REmpty. Or even at the RE level by applying laws, such as
-- liftA2 f REmpty x = REmpty or liftA2 f (RPure x) y = RFmap (f x) y.
-- I don't know yet if this is worth doing.
