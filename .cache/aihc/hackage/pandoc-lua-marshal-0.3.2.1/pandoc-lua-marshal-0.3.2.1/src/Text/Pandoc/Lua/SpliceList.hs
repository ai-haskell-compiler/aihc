{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module      : Text.Pandoc.Lua.Walk
Copyright   : © 2012-2021 John MacFarlane,
              © 2017-2021 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Walking documents in a filter-suitable way.
-}
module Text.Pandoc.Lua.SpliceList
  ( SpliceList (..)
  )
where

import Control.Monad ((<=<))
import Text.Pandoc.Definition
import Text.Pandoc.Walk

-- | Helper type which allows to traverse trees in order, while splicing
-- in trees.
--
-- The only interesting use of this type is via it's '@Walkable@'
-- instance. That instance makes it possible to walk a Pandoc document
-- (or a subset thereof), while applying a function on each element of
-- an AST element /list/, and have the resulting list spliced back in
-- place of the original element. This is the traversal/splicing method
-- used for Lua filters.
newtype SpliceList a = SpliceList { unSpliceList :: [a] }
  deriving stock (Functor, Foldable, Traversable)

--
-- SpliceList Inline
--
instance {-# OVERLAPPING #-} Walkable (SpliceList Inline) [Inline] where
  walkM = walkSpliceListM
  query = querySpliceList

instance Walkable (SpliceList Inline) Pandoc where
  walkM = walkPandocM
  query = queryPandoc

instance Walkable (SpliceList Inline) Citation where
  walkM = walkCitationM
  query = queryCitation

instance Walkable (SpliceList Inline) Inline where
  walkM = walkInlineM
  query = queryInline

instance Walkable (SpliceList Inline) Block where
  walkM = walkBlockM
  query = queryBlock

instance Walkable (SpliceList Inline) Row where
  walkM = walkRowM
  query = queryRow

instance Walkable (SpliceList Inline) TableHead where
  walkM = walkTableHeadM
  query = queryTableHead

instance Walkable (SpliceList Inline) TableBody where
  walkM = walkTableBodyM
  query = queryTableBody

instance Walkable (SpliceList Inline) TableFoot where
  walkM = walkTableFootM
  query = queryTableFoot

instance Walkable (SpliceList Inline) Caption where
  walkM = walkCaptionM
  query = queryCaption

instance Walkable (SpliceList Inline) Cell where
  walkM = walkCellM
  query = queryCell

instance Walkable (SpliceList Inline) MetaValue where
  walkM = walkMetaValueM
  query = queryMetaValue

instance Walkable (SpliceList Inline) Meta where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap

--
-- SpliceList Block
--
instance {-# OVERLAPPING #-} Walkable (SpliceList Block) [Block] where
  walkM = walkSpliceListM
  query = querySpliceList

instance Walkable (SpliceList Block) Pandoc where
  walkM = walkPandocM
  query = queryPandoc

instance Walkable (SpliceList Block) Citation where
  walkM = walkCitationM
  query = queryCitation

instance Walkable (SpliceList Block) Inline where
  walkM = walkInlineM
  query = queryInline

instance Walkable (SpliceList Block) Block where
  walkM = walkBlockM
  query = queryBlock

instance Walkable (SpliceList Block) Row where
  walkM = walkRowM
  query = queryRow

instance Walkable (SpliceList Block) TableHead where
  walkM = walkTableHeadM
  query = queryTableHead

instance Walkable (SpliceList Block) TableBody where
  walkM = walkTableBodyM
  query = queryTableBody

instance Walkable (SpliceList Block) TableFoot where
  walkM = walkTableFootM
  query = queryTableFoot

instance Walkable (SpliceList Block) Caption where
  walkM = walkCaptionM
  query = queryCaption

instance Walkable (SpliceList Block) Cell where
  walkM = walkCellM
  query = queryCell

instance Walkable (SpliceList Block) MetaValue where
  walkM = walkMetaValueM
  query = queryMetaValue

instance Walkable (SpliceList Block) Meta where
  walkM f (Meta metamap) = Meta <$> walkM f metamap
  query f (Meta metamap) = query f metamap


walkSpliceListM :: (Monad m, Walkable (SpliceList a) a)
                => (SpliceList a -> m (SpliceList a))
                -> [a] -> m [a]
walkSpliceListM f =
  let f' = fmap unSpliceList . f . SpliceList . (:[]) <=< walkM f
  in fmap mconcat . mapM f'

querySpliceList :: (Monoid c, Walkable (SpliceList a) a)
                => (SpliceList a -> c)
                -> [a] -> c
querySpliceList f =
  let f' x = f (SpliceList [x]) `mappend` query f x
  in mconcat . map f'
