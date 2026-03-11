{-# LANGUAGE NoImplicitPrelude #-}

module Data.TypeMap.Dynamic
  ( -- * Dynamic type maps

    TypeMap()
  , Item

    -- ** Basic operations

  , empty
  , null
  , size
  , insert
  , (<:)
  , update
  , alter
  , lookup
  , delete

    -- ** Traversals and folds

  , map
  , traverse
  , toList
  , toListMap

    -- ** Set-like operations

  , union
  , difference
  , intersection

    -- * Type-level mappings

  , OfType
  ) where

import Data.TypeMap.Internal.Dynamic
