{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{- |
Module      : Text.Pandoc.Lua.Walk
Copyright   : © 2012-2021 John MacFarlane,
              © 2017-2021 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Walking documents in a filter-suitable way.
-}
module Text.Pandoc.Lua.Walk
  ( SpliceList (..)
  , Walkable
  , TraversalControl (..)
  , walkSplicing
  , walkStraight
  , applyStraight
  , applySplicing
  , applyStraightFunction
  , applySplicingFunction
  )
where

import Prelude hiding (lookup)
import Control.Applicative ((<|>))
import Control.Monad ((<$!>))
import Data.Data (Data)
import Data.Proxy (Proxy (..))
import HsLua
import Text.Pandoc.Lua.Marshal.Filter
import Text.Pandoc.Lua.SpliceList (SpliceList (..))
import Text.Pandoc.Walk

--
-- Straight
--

-- | Walks an element, modifying all values of type @a@ by applying the
-- given Lua 'Filter'.
walkStraight :: forall e a b. (LuaError e, Walkable a b)
             => Name  -- ^ Name under which the filter function is stored
             -> Pusher e a
             -> Peeker e a
             -> Filter
             -> b -> LuaE e b
walkStraight filterFnName pushElement peekElement filter' =
  case filterFnName `lookup` filter' of
    Nothing ->
      -- There is no filter function, do nothing.
      pure
    Just fn ->
      -- Walk the element with the filter function.
      walkM $ fmap fst . applyStraightFunction fn pushElement peekElement

-- | Applies a filter on an element. The element is pushed to the stack
-- via the given pusher and calls the filter function with that value,
-- leaving the filter function's return value on the stack.
applyStraight :: (LuaError e, Data a)
              => Pusher e a -> Peeker e a -> Filter
              -> a -> LuaE e a
applyStraight pushElement peekElement filter' x = do
  case filter' `getFunctionFor` x of
    Nothing ->
      -- There is no filter function, do nothing.
      pure x
    Just fn -> fst <$>
      -- Apply the function
      applyStraightFunction fn pushElement peekElement x

-- | Applies a single filter function on an element. The element is
-- pushed to the stack via the given pusher and calls the filter
-- function with that value, leaving the filter function's return value
-- on the stack.
applyStraightFunction :: LuaError e
                      => FilterFunction -> Pusher e a -> Peeker e a
                      -> a -> LuaE e (a, TraversalControl)
applyStraightFunction fn pushElement peekElement x = do
  pushFilterFunction fn
  pushElement x
  callTrace 1 2
  forcePeek . (`lastly` pop 2) $
    (,)
    <$> ((x <$ peekNil (nth 2)) <|> peekElement (nth 2))
    <*> peekTraversalControl top

--
-- Splicing
--

-- | Walks an element, using a Lua 'Filter' to modify all values of type
-- @a@ that are in a list. The result of the called filter function must
-- be a retrieved as a list, and it is spliced back into the list at the
-- position of the original element. This allows to delete an element,
-- or to replace an element with multiple elements.
walkSplicing :: forall e a b. (LuaError e, Data a, Walkable (SpliceList a) b)
             => Pusher e a
             -> Peeker e [a]
             -> Filter
             -> b -> LuaE e b
walkSplicing pushElement peekElementOrList filter' =
  if any (`member` filter') acceptedNames
  then walkM $ \(SpliceList xs) -> SpliceList <$!> fmap mconcat (mapM f xs)
  else pure
 where
  f :: a -> LuaE e [a]
  f = applySplicing pushElement peekElementOrList filter'

  acceptedNames :: [Name]
  acceptedNames = baseFunctionName (Proxy @a) : valueFunctionNames (Proxy @a)

-- | Applies a filter on an element. The element is pushed to the stack
-- via the given pusher and calls the filter function with that value,
-- leaving the filter function's return value on the stack.
applySplicing :: (LuaError e, Data a)
              => Pusher e a -> Peeker e [a] -> Filter
              -> a -> LuaE e [a]
applySplicing pushElement peekElements filter' x = do
  case filter' `getFunctionFor` x of
    Nothing ->
      -- There is no filter function, do nothing.
      pure [x]
    Just fn -> fst <$>
      -- Apply the function
      applySplicingFunction fn pushElement peekElements x

-- | Applies a single filter function on an element. The element is
-- pushed to the stack via the given pusher and calls the filter
-- function with that value, leaving the filter function's return value
-- on the stack.
applySplicingFunction :: LuaError e
                      => FilterFunction -> Pusher e a -> Peeker e [a]
                      -> a -> LuaE e ([a], TraversalControl)
applySplicingFunction fn pushElement peekElements x = do
  pushFilterFunction fn
  pushElement x
  callTrace 1 2
  forcePeek . (`lastly` pop 2) $
    (,)
    <$> (liftLua (ltype (nth 2)) >>= \case
            TypeNil -> pure [x]  -- function returned `nil`, keep original value
            _       -> peekElements (nth 2))
    <*> peekTraversalControl top

--
-- Helper
--

data TraversalControl = Continue | Stop

-- | Retrieves a Traversal control value: @nil@ or a truthy value
-- translate to 'Continue', @false@ is treated to mean 'Stop'.
peekTraversalControl :: Peeker e TraversalControl
peekTraversalControl idx = (Continue <$ peekNil idx)
  <|> (liftLua (toboolean top) >>= \case
          True -> pure Continue
          False -> pure Stop)
