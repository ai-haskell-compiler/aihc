{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Lentil.File
-- Copyright   :  © 2015 Francesco Ariis, Tomislav
-- License     :  GPLv3 (see the LICENSE file)
--
-- File operations
-----------------------------------------------------------------------------

module Lentil.File where

import Lentil.Types
import Lentil.Parse.Run
import Lentil.Parse.Syntaxes

import System.FilePath
import System.FilePath.Find
import Data.Semigroup as S
import Data.Monoid as M ()

import qualified Data.Char as C
import qualified Data.List as L


---------------
-- INSTANCES --
---------------

instance Semigroup a => Semigroup (FindClause a) where
    (<>) = liftA2 (S.<>)

instance (Semigroup a, Monoid a) => Monoid (FindClause a) where
    mappend = (S.<>)
    mempty  = pure mempty


--------------
-- FILESCAN --
--------------

findIssues :: [Alias] -> [FlagWord] ->
              [FilePath] -> [FilePath] -> IO [Issue]
findIssues as fws fps xs =
        findFiles as fps xs   >>= \fl ->
        issueFinder as fws fl >>= \r  ->
        return r

-- actual find function for IO ()
findFiles :: [Alias] -> [FilePath] -> [FilePath] -> IO [FilePath]
findFiles as fps xs = fmap concat (mapM fc fps)
    where
          fc i = find recPred (findClause (xs' i) as) i -- search pattern

          xs' "." = map (combine ".") xs -- trick to exclude on '.'
          xs' _   = xs


-- fp to exclude, clause (+ aliases, to include)
findClause :: [FilePath] -> [Alias] -> FindClause Bool
findClause xs as = let xc = mconcat $ map fp2fc xs
                   in fileType ==? RegularFile &&?
                      extCheck                 &&?
                      (not <$> fmap getAny xc)
    where
          fp2fc :: FilePath -> FindClause Any
          fp2fc f = Any . L.isPrefixOf f <$> filePath
          -- TODO: combine funziona su windows? [test]

          extCheck = (fmap getAny . mconcat)
                     (map (fmap Any . (extension ~=?)) (extensionList as))

          (~=?) = let tl s = map C.toLower s
                  in  liftOp (\a b -> tl a == tl b)

-- recursion predicate: excludes dot ('.') or _ folders
recPred :: RecursionPredicate
recPred = not . isDotFolder <$> fileName
    where
          isDotFolder "." = False -- not curr dir!
          isDotFolder fp  | null fp                     = False
                          | L.elem (head fp) ['.', '_'] = True
          isDotFolder _   = False

