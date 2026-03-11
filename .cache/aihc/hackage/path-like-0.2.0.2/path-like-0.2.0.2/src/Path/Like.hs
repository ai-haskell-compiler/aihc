{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{- |
   Module      : Path.Like
   License     : MIT
   Stability   : experimental

Type classes for compiling down to well-typed `Path`s.
-}

module Path.Like (
  PathLike(..)
, FileLike(..)
, DirLike(..)
, (/>)
) where

import Path

-- | Class representing a type `a` that can be compiled down to a `Path b t`.
class PathLike b t a | a -> b, a -> t where
  toPath :: a -> Path b t

-- | Class representing a type `a` that can be compiled down to a `Path b File`.
class PathLike b File a => FileLike b a where
  toFile :: a -> Path b File
  toFile = toPath

-- | Class repreenting a type `a` that can be compiled down to a `Path b Dir`.
class PathLike b Dir a => DirLike b a where
  toDir :: a -> Path b Dir
  toDir = toPath

instance PathLike b t (Path b t) where
  toPath = id

instance FileLike b (Path b File)
instance DirLike b (Path b Dir)

-- | Like `Path.</>`, but works for any `DirLike` and relative `FileLike` to produce a concrete `Path`.
(/>) :: (DirLike b a, FileLike Rel c) => a -> c -> Path b File
(/>) x y = toDir x </> toFile y
