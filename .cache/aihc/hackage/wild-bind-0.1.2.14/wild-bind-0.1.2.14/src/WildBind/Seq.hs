{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- |
-- Module: WildBind.Seq
-- Description: Support for binding sequence of input events.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module defines convenient functions to build 'Binding's that
-- bind actions to key sequences.
--
-- For example, see
-- [WildBind.Task.X11.Seq.Example in wild-bind-task-x11](https://hackage.haskell.org/package/wild-bind-task-x11/docs/WildBind-Task-X11-Seq-Example.html) package.
--
-- @since 0.1.1.0
--

module WildBind.Seq
    ( -- * Simple API
      prefix
      -- * Advanced API
    , SeqBinding
    , toSeq
    , fromSeq
    , withPrefix
    , withCancel
    , reviseSeq
    ) where

import           Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import           Data.Monoid               (Monoid (..), mconcat)
import           Data.Semigroup            (Semigroup (..))

import           WildBind.Binding          (Action, Binding, Binding', as, binds', extend,
                                            justBefore, on, revise, revise', run, startFrom,
                                            whenBack)

-- | Intermediate type of building a 'Binding' for key sequences.
newtype SeqBinding fs i
  = SeqBinding ([i] -> Binding' [i] fs i)

-- | Follows the same rule as 'Binding'.
instance Ord i => Semigroup (SeqBinding fs i) where
  (SeqBinding a) <> (SeqBinding b) =
    SeqBinding $ \ps -> mappend (a ps) (b ps)

-- | Follows the same rule as 'Binding'.
instance Ord i => Monoid (SeqBinding fs i) where
  mempty = SeqBinding $ const mempty
  mappend = (<>)

-- | Prepend prefix keys to the 'SeqBinding'.
--
-- 'SeqBinding' is composable in terms of prefixes, that is,
--
-- > (withPrefix [key1, key2] seq_b) == (withPrefix [key1] $ withPrefix [key2] seq_b)
withPrefix :: Ord i
           => [i] -- ^ prefix keys
           -> SeqBinding fs i
           -> SeqBinding fs i
withPrefix ps sb = foldr withPrefixSingle sb ps

withPrefixSingle :: Ord i => i -> SeqBinding fs i -> SeqBinding fs i
withPrefixSingle p (SeqBinding fb) =
  SeqBinding $ \cur_prefix -> nextBinding cur_prefix <> prefixBinding cur_prefix
  where
    prefixBinding cur_prefix = whenBack (== cur_prefix) $ binds' $ do
      on p `as` "prefix" `run` State.modify (++ [p])
    nextBinding cur_prefix = fb (cur_prefix ++ [p])

-- | Create a 'SeqBinding' from 'Binding'. The result 'SeqBinding' has
-- no prefixes yet.
toSeq :: Eq i => Binding fs i -> SeqBinding fs i
toSeq b = SeqBinding $ \ps -> whenBack (== ps) $ revise' cancelBefore $ extend b
  where
    cancelBefore _ _ _ = justBefore $ State.put []

-- | Resolve 'SeqBinding' to build a 'Binding' for key sequences.
fromSeq :: SeqBinding fs i -> Binding fs i
fromSeq (SeqBinding fb) = startFrom [] $ fb []

-- | A 'SeqBinding' that binds the given key for canceling the key
-- sequence.
cancelOn :: Ord i
         => i -- ^ cancel key
         -> SeqBinding fs i
cancelOn c = SeqBinding $ const $ whenBack (not . null) $ binds' $ on c `as` "cancel" `run` State.put []

-- | Add cancel keys to the 'SeqBinding'.
withCancel :: Ord i
           => [i] -- ^ cancel keys
           -> SeqBinding fs i
           -> SeqBinding fs i
withCancel cs sb = cancelBindings <> sb
  where
    cancelBindings = mconcat $ map cancelOn cs

-- | Prepend prefix keys to a 'Binding'. In the result 'Binding', the
-- original 'Binding' is enabled only after you input the prefix input
-- symbols in the same order.
--
-- During typing prefix keys, you can cancel and reset the key
-- sequence by typing the \"cancel keys\". This is analogous to @C-g@
-- in Emacs. The binding of cancel keys are weak, that is, they are
-- overridden by the original binding and prefix keys.
--
-- Note that this function creates an independent implicit state to
-- memorize prefix keys input so far. This means,
--
-- > (prefix [] [key1, key2] b) /= (prefix [] [key1] $ prefix [] [key2] b)
--
-- If you want a more composable way of building a sequence binding,
-- try 'SeqBinding'.
prefix :: Ord i
       => [i] -- ^ The cancel keys (input symbols for canceling the current key sequence.)
       -> [i] -- ^ list of prefix input symbols
       -> Binding fs i -- ^ the original binding.
       -> Binding fs i -- ^ the result binding.
prefix cs ps = fromSeq . withCancel cs . withPrefix ps . toSeq

-- | Revise actions in 'SeqBinding'. See 'WildBind.Binding.revise'.
reviseSeq :: (forall a . [i] -> fs -> i -> Action IO a -> Maybe (Action IO a))
             -- ^ Revising function. @[i]@ is the prefix keys input so far.
          -> SeqBinding fs i
          -> SeqBinding fs i
reviseSeq f (SeqBinding orig) = SeqBinding $ fmap (revise f) orig
