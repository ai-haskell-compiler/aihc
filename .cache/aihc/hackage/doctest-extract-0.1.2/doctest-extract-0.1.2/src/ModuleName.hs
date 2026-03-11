module ModuleName (
   T(..),
   singleton,
   parse,
   dirPath,
   filePath,
   filePaths,
   ) where

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Foldable as Fold
import Data.NonEmpty ((!:))
import Data.Semigroup (Semigroup, (<>))

import qualified System.Path.PartClass as PathClass
import qualified System.Path as Path
import System.Path (joinPath, (<.>))

import Control.Monad (when)


data T = Cons {
   string :: String,
   components :: NonEmpty.T [] String
   } deriving (Show)

instance Semigroup T where
   Cons name0 components0 <> Cons name1 components1 =
      Cons
         (name0 ++ '.' : name1)
         (NonEmptyC.append components0 components1)

singleton :: String -> T
singleton name = Cons name (NonEmpty.singleton name)

parse :: String -> Either String T
parse name = do
   let comps = NonEmpty.chop ('.'==) name
   when (Fold.any null comps) (Left "empty module name component")
   return (Cons name comps)


genericPath :: (PathClass.FileDir fd) => T -> Path.Rel fd
genericPath = joinPath . NonEmpty.flatten . components

dirPath :: T -> Path.RelDir
dirPath = genericPath

filePath :: T -> Path.RelFile
filePath name = genericPath name <.> "hs"

filePaths :: T -> NonEmpty.T [] Path.RelFile
filePaths name = fmap (genericPath name <.>) ("hs" !: "lhs" : [])
