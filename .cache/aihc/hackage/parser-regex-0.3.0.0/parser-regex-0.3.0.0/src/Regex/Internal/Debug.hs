{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides functions for visualizing @RE@s and @Parser@s.
-- [See here](https://github.com/meooow25/parser-regex/wiki/Visualizations)
-- for some examples.
--
module Regex.Internal.Debug
  ( reToDot
  , parserToDot
  , dispCharRanges
  ) where

import Control.Monad ((>=>))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.State.Strict
  (StateT(..), evalStateT, gets, modify', state)
import Control.Monad.Trans.Writer.CPS (Writer, execWriter, tell)
import qualified Data.Foldable as F
import Data.Maybe (isJust)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Regex.Internal.Regex (RE(..))
import Regex.Internal.Parser (Node(..), Parser(..))
import Regex.Internal.Unique (Unique(..))
import qualified Regex.Internal.CharSet as CS

-------
-- RE
-------

-- | Generate a [Graphviz DOT](https://graphviz.org/doc/info/lang.html)
-- visualization of a 'RE'. Optionally takes an alphabet @[c]@, which will be
-- tested against the @token@ functions in the 'RE' and accepted characters
-- displayed.
reToDot :: forall c a. Maybe ([c], [c] -> String) -> RE c a -> String
reToDot ma re0 = execM $ do
  writeLn (str "digraph RE {")
  _ <- go re0
  writeLn (str "}")
  where
    go :: forall b. RE c b -> M Id
    go re = case re of
      RToken t -> new $ labelToken "RToken" t ma
      RFmap _ re1 ->
        withNew (str "RFmap") $ \i ->
          go re1 >>= writeEdge i
      RFmap_ _ re1 ->
        withNew (str "RFmap_") $ \i ->
          go re1 >>= writeEdge i
      RPure _ -> new (str "RPure")
      RLiftA2 _ re1 re2 ->
        withNew (str "RLiftA2") $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
      REmpty -> new (str "REmpty")
      RAlt re1 re2 ->
        withNew (str "RAlt") $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
      RFoldGr _ _ re1 ->
        withNew (str "RFoldGr") $ \i ->
          go re1 >>= writeEdge i
      RFoldMn _ _ re1 ->
        withNew (str "RFoldMn") $ \i ->
          go re1 >>= writeEdge i
      RMany _ _ _ _ re1 ->
        withNew (str "RMany") $ \i ->
          go re1 >>= writeEdge i

-----------
-- Parser
-----------

-- | Generate a [Graphviz DOT](https://graphviz.org/doc/info/lang.html)
-- visualization of a 'Parser'. Optionally takes an alphabet @[c]@, which will
-- be tested against the @token@ functions in the 'Parser' and the accepted
-- characters displayed.
parserToDot :: forall c a. Maybe ([c], [c] -> String) -> Parser c a -> String
parserToDot ma p0 = execM $ do
  writeLn (str "digraph Parser {")
  _ <- go p0
  writeLn (str "}")
  where
    go :: forall b. Parser c b -> M Id
    go p = case p of
      PToken t -> new $ labelToken "PToken" t ma
      PFmap _ re1 ->
        withNew (str "PFmap") $ \i ->
          go re1 >>= writeEdge i
      PFmap_ node ->
        withNew (str "PFmap_") $ \i -> do
          writeLn $ str "subgraph cluster" <> idStr i <> str " {"
          j <- evalStateT (goNode node) IM.empty
          writeLn (str "}")
          writeEdge i j
      PPure _ -> new (str "PPure")
      PLiftA2 _ re1 re2 ->
        withNew (str "PLiftA2") $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
      PEmpty -> new (str "PEmpty")
      PAlt _ re1 re2 res ->
        withNew (str "PAlt") $ \i -> do
          go re1 >>= writeEdge i
          go re2 >>= writeEdge i
          F.traverse_ (go >=> writeEdge i) res
      PMany _ _ _ _ _ re1 ->
        withNew (str "PMany") $ \i ->
          go re1 >>= writeEdge i
      PFoldGr _ _ _ re1 ->
        withNew (str "PFoldGr") $ \i ->
          go re1 >>= writeEdge i
      PFoldMn _ _ _ re1 ->
        withNew (str "PFoldMn") $ \i ->
          go re1 >>= writeEdge i

    goNode :: forall b. Node c b -> StateT (IntMap Id) M Id
    goNode n = case n of
      NAccept _ -> lift $ new (str "NAccept")
      NGuard u n1 -> do
        v <- gets $ IM.lookup (unUnique u)
        case v of
          Just i -> pure i
          Nothing -> withNewT (str "NGuard") $ \i -> do
            modify' $ IM.insert (unUnique u) i
            goNode n1 >>= lift . writeEdge i
      NToken t n1 ->
        withNewT (labelToken "NToken" t ma) $ \i ->
          goNode n1 >>= lift . writeEdge i
      NEmpty -> lift $ new (str "NEmpty")
      NAlt n1 n2 ns -> withNewT (str "NAlt") $ \i -> do
        goNode n1 >>= lift . writeEdge i
        goNode n2 >>= lift . writeEdge i
        F.traverse_ (goNode >=> lift . writeEdge i) ns

------------------
-- Display Chars
------------------

-- |
-- >>> dispCharRanges "abc012def"
-- "[('0','2'),('a','f')]"
dispCharRanges :: [Char] -> String
dispCharRanges = show . CS.ranges . CS.fromList

-----------------
-- Common stuff
-----------------

newtype Str = Str { runStr :: String -> String }

str :: String -> Str
str = Str . (++)

instance Semigroup Str where
  s1 <> s2 = Str (runStr s1 . runStr s2)

instance Monoid Str where
  mempty = Str id

labelToken :: String -> (c -> Maybe a) -> Maybe ([c], [c] -> String) -> Str
labelToken node t = maybe
  (str node)
  (\(cs, disp) -> str node <+> (str . escape . disp) (filter (isJust . t) cs))

escape :: String -> String
escape = init . tail' . show
  where
    tail' (_:xs) = xs
    tail' [] = error "tail'"

(<+>) :: Str -> Str -> Str
s1 <+> s2 = s1 <> str " " <> s2
infixr 6 <+>

declNode :: Id -> Str -> Str
declNode i label =
  idStr i <+>
  str "[label=\"" <>
  label <>
  str "\", ordering=\"out\"]"

type M = StateT Int (Writer Str)

execM :: M a -> String
execM = ($ "") . runStr . execWriter . flip runStateT 1

newtype Id = Id { unId :: String }

idStr :: Id -> Str
idStr = str . unId

nxt :: M Id
nxt = state $ \i -> let !i' = i+1 in (Id (show i), i')

writeLn :: Str -> M ()
writeLn = lift . tell . (<> str "\n")

writeEdge :: Id -> Id -> M ()
writeEdge fr to = writeLn $ idStr fr <> str " -> " <> idStr to

new :: Str -> M Id
new node = do
  i <- nxt
  writeLn $ declNode i node
  pure i

withNew :: Str -> (Id -> M a) -> M Id
withNew node f = runIdentityT $ withNewT node $ lift . f

withNewT :: (MonadTrans t, Monad (t M)) => Str -> (Id -> t M a) -> t M Id
withNewT node f = do
  i <- lift $ new node
  _ <- f i
  pure i
