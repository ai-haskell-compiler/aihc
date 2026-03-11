{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A basic query facility.
module Zenacy.HTML.Internal.Query
  ( HTMLQuery
  , htmlQueryRun
  , htmlQueryExec
  , htmlQueryTry
  , htmlQueryStop
  , htmlQueryCont
  , htmlQuerySucc
  , htmlQueryZipper
  , htmlQueryNode
  , htmlQueryFirst
  , htmlQueryLast
  , htmlQueryNext
  , htmlQueryPrev
  , htmlQueryUp
  , htmlQueryTest
  , htmlQueryName
  , htmlQueryIsFirst
  , htmlQueryIsLast
  , htmlQuerySave
  , htmlQueryGet
  , htmlQueryGetZipper
  , htmlQuerySrc
  , htmlQueryAttr
  , htmlQueryAttrVal
  , htmlQueryId
  , htmlQueryHasClass
  , htmlQueryOnly
  ) where

import Prelude
import Zenacy.HTML.Internal.HTML
import Zenacy.HTML.Internal.Oper
import Zenacy.HTML.Internal.Zip
import Control.Monad.State
  ( MonadState
  , State
  , evalState
  , modify
  , gets
  )
import Control.Monad.Trans.Maybe
  ( MaybeT(..)
  , runMaybeT
  )
import Data.Bool
  ( bool
  )
import Data.IntMap
  ( IntMap
  )
import qualified Data.IntMap as IntMap
  ( fromList
  , lookup
  , insert
  )
import Data.Maybe
  ( fromMaybe
  , isNothing
  )
import Data.Text
  ( Text
  )

-- | Defines the query state.
type QueryState = (HTMLZipper, IntMap HTMLZipper)

-- | Defines the type for a query.
newtype HTMLQuery a = HTMLQuery { htmlQueryState :: MaybeT (State QueryState) a }
  deriving (Functor, Applicative, Monad, MonadState QueryState)

-- | Runs a query and returns a result.
htmlQueryRun :: HTMLNode -> HTMLQuery a -> Maybe a
htmlQueryRun x q = evalState (runMaybeT $ htmlQueryState q) s
  where
    z = htmlZip x
    s = (z, IntMap.fromList [(0,z)])

-- | Same as run with the arguments flipped.
htmlQueryExec :: HTMLQuery a -> HTMLNode -> Maybe a
htmlQueryExec = flip htmlQueryRun

-- | Same as run with the arguments flipped.
htmlQueryTry :: HTMLQuery HTMLNode -> HTMLNode -> HTMLNode
htmlQueryTry q x = fromMaybe x $ htmlQueryRun x q

-- | Wraps a value as a query result.
htmlQueryWrap :: Maybe a -> HTMLQuery a
htmlQueryWrap = HTMLQuery . MaybeT . pure

-- | Returns a result that stops the query.
htmlQueryStop :: HTMLQuery a
htmlQueryStop = htmlQueryWrap $ Nothing

-- | Returns a result that continues the query.
htmlQueryCont :: HTMLQuery ()
htmlQueryCont = htmlQuerySucc ()

-- | Returns a successful query result.
htmlQuerySucc :: a -> HTMLQuery a
htmlQuerySucc = htmlQueryWrap . Just

-- | Gets the current query zipper.
htmlQueryZipper :: HTMLQuery HTMLZipper
htmlQueryZipper = gets fst

-- | Gets the current query node.
htmlQueryNode :: HTMLQuery HTMLNode
htmlQueryNode = htmlZipNode <$> htmlQueryZipper

-- | Performs a query step with a zipper operation.
withZip :: (HTMLZipper -> Maybe HTMLZipper) -> HTMLQuery ()
withZip f = do
  z <- htmlQueryZipper
  case f z of
    Nothing ->
      htmlQueryStop
    Just z' -> do
      modify $ \(_, m) -> (z', m)
      htmlQueryCont

-- | Moves the query to the first child node.
htmlQueryFirst :: HTMLQuery ()
htmlQueryFirst = withZip htmlZipFirst

-- | Moves the query to the last child node.
htmlQueryLast :: HTMLQuery ()
htmlQueryLast = withZip htmlZipLast

-- | Moves the query to the next sibling node.
htmlQueryNext :: HTMLQuery ()
htmlQueryNext = withZip htmlZipNext

-- | Moves the query to the previous sibling node.
htmlQueryPrev :: HTMLQuery ()
htmlQueryPrev = withZip htmlZipPrev

-- | Moves the query to the parent node.
htmlQueryUp :: HTMLQuery ()
htmlQueryUp = withZip htmlZipUp

-- | Evaluates a test result and continues the query if true.
htmlQueryTest :: Bool -> HTMLQuery ()
htmlQueryTest = bool htmlQueryStop htmlQueryCont

-- | Tests the current element name.
htmlQueryName :: Text -> HTMLQuery ()
htmlQueryName x = do
  n <- htmlQueryNode
  htmlQueryTest $ htmlElemName n == x

-- | Tests the current node to see if it is the first sibling.
htmlQueryIsFirst :: HTMLQuery ()
htmlQueryIsFirst = do
  z <- htmlQueryZipper
  htmlQueryTest $ isNothing $ htmlZipPrev z

-- | Tests the current node to see if it is the last sibling.
htmlQueryIsLast :: HTMLQuery ()
htmlQueryIsLast = do
  z <- htmlQueryZipper
  htmlQueryTest $ isNothing $ htmlZipNext z

-- | Saves the current query state.
htmlQuerySave :: Int -> HTMLQuery ()
htmlQuerySave x = do
  modify $ \(z, m) -> (z, IntMap.insert x z m)
  htmlQueryCont

-- | Gets a saved query node.
htmlQueryGet :: Int -> HTMLQuery HTMLNode
htmlQueryGet x = htmlZipNode <$> htmlQueryGetZipper x

-- | Gets a saved query zipper.
htmlQueryGetZipper :: Int -> HTMLQuery HTMLZipper
htmlQueryGetZipper x = do
  m <- gets snd
  case IntMap.lookup x m of
    Just z -> htmlQuerySucc z
    Nothing -> htmlQueryStop

-- | Gets the source input node.
htmlQuerySrc :: HTMLQuery HTMLNode
htmlQuerySrc = htmlQueryGet 0

-- | Tests if the current node has an attribute.
htmlQueryAttr :: Text -> HTMLQuery ()
htmlQueryAttr x = htmlQueryNode >>= htmlQueryTest . htmlElemHasAttrName x

-- | Tests if the current node has an attribute value.
htmlQueryAttrVal :: Text -> Text -> HTMLQuery ()
htmlQueryAttrVal n v = htmlQueryNode >>= htmlQueryTest . htmlElemHasAttrVal n v

-- | Tests if the current node has an id.
htmlQueryId :: Text -> HTMLQuery ()
htmlQueryId x = htmlQueryNode >>= htmlQueryTest . htmlElemHasID x

-- | Tests if the current node has a class.
htmlQueryHasClass :: Text -> HTMLQuery ()
htmlQueryHasClass x = htmlQueryNode >>= htmlQueryTest . htmlElemClassesContains x

-- | Moves to the child and require that it is the only child.
htmlQueryOnly :: Text -> HTMLQuery ()
htmlQueryOnly x = htmlQueryFirst >> htmlQueryName x >> htmlQueryIsLast
