{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module      : Text.Pandoc.Lua.Topdown
Copyright   : © 2012-2021 John MacFarlane,
              © 2017-2021 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Walk documents in a filter-suitable way, descending from the root
towards the leaves.
-}
module Text.Pandoc.Lua.Topdown
  ( TraversalNode (..)
  , Topdown (..)
  , TraversalControl (..)
  )
where

import Control.Monad ((>=>))
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Walk
import Text.Pandoc.Walk

-- | Helper type to do a preorder traversal of a subtree.
data TraversalNode
  = TBlock Block
  | TBlocks [Block]
  | TInline Inline
  | TInlines [Inline]

-- | Type used to traverse a 'Pandoc' AST from top to bottom, i.e.,
-- processing the root element first and then continue towards the
-- leaves depth-first. Aborts the descend if 'topdownControl' is 'Stop'.
data Topdown = Topdown
  { topdownControl :: TraversalControl
  , topdownNode :: TraversalNode
  }

-- | Extracts a list of 'Inline' elements from a 'TraversalNode'.
-- WARNING: This is a partial function and will throw an error if the
-- node contains a 'Block' or a list of 'Block's.
nodeInlines :: TraversalNode -> [Inline]
nodeInlines = \case
  TInlines xs -> xs
  TInline x   -> [x]
  _            -> error $ "The 'impossible' has happened."
                       ++ "Please report this as a bug"

-- | Extracts a list of 'Block' elements from a 'TraversalNode'.
nodeBlocks :: TraversalNode -> [Block]
nodeBlocks = \case
  TBlocks xs  -> xs
  TBlock x    -> [x]
  TInlines xs -> [Plain xs]
  TInline x   -> [Plain [x]]

-- | Creates a topdown-walking function for a list of elements.
walkTopdownM :: (Monad m, Walkable Topdown a)
             => ([a] -> TraversalNode)
             -> (a -> TraversalNode)
             -> (TraversalNode -> [a])
             -> (Topdown -> m Topdown)
             -> [a] -> m [a]
walkTopdownM mkListNode mkElemNode nodeToList f =
  f . Topdown Continue . mkListNode >=> \case
    Topdown Stop     node -> return $ nodeToList node
    Topdown Continue node -> mconcat <$>
      traverse (f . Topdown Continue . mkElemNode >=> \case
                   Topdown Stop     node' -> return $ nodeToList node'
                   Topdown Continue node' -> traverse (walkM f) $
                                             nodeToList node')
               (nodeToList node)

-- | Creates a topdown-query function for a list of elements.
queryTopdown :: (Monoid a, Walkable Topdown b)
             => ([b] -> TraversalNode)
             -> (Topdown -> a) -> [b] -> a
queryTopdown mkListNode f xs =
  f (Topdown Continue $ mkListNode xs) <> mconcat (map (query f) xs)

instance {-# OVERLAPPING #-} Walkable Topdown [Block] where
  walkM = walkTopdownM TBlocks TBlock nodeBlocks
  query = queryTopdown TBlocks

instance {-# OVERLAPPING #-} Walkable Topdown [Inline] where
  walkM = walkTopdownM TInlines TInline nodeInlines
  query = queryTopdown TInlines

instance Walkable Topdown Block where
  walkM = walkBlockM
  query = queryBlock

instance Walkable Topdown Inline where
  walkM = walkInlineM
  query = queryInline

instance Walkable Topdown Pandoc where
  walkM = walkPandocM
  query = queryPandoc

instance Walkable Topdown Citation where
  walkM = walkCitationM
  query = queryCitation

instance Walkable Topdown Row where
  walkM = walkRowM
  query = queryRow

instance Walkable Topdown TableHead where
  walkM = walkTableHeadM
  query = queryTableHead

instance Walkable Topdown TableBody where
  walkM = walkTableBodyM
  query = queryTableBody

instance Walkable Topdown TableFoot where
  walkM = walkTableFootM
  query = queryTableFoot

instance Walkable Topdown Caption where
  walkM = walkCaptionM
  query = queryCaption

instance Walkable Topdown Cell where
  walkM = walkCellM
  query = queryCell

instance Walkable Topdown MetaValue where
  walkM = walkMetaValueM
  query = queryMetaValue

instance Walkable Topdown Meta where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap
