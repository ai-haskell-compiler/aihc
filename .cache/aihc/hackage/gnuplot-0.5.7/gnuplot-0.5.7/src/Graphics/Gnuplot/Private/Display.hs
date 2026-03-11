module Graphics.Gnuplot.Private.Display where

import qualified Graphics.Gnuplot.Private.FrameOption as Option
import qualified Graphics.Gnuplot.File as File

import qualified Data.Map as Map
import qualified Control.Monad.Trans.Reader as MR
import qualified Control.Monad.Trans.State as MS
import Control.Monad (liftM2, return, )
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )

import Prelude (FilePath, String, Int, ($), (.), )


newtype Script =
   Script {
      runScript :: MS.StateT (Int, OptionSet) (MR.Reader FilePath) Body
   }

pure :: Body -> Script
pure = Script . return

data Body =
   Body {
      files :: [File.T],
      commands :: [String]
   }

type OptionSet = Map.Map Option.T [String]


instance Semigroup Script where
   Script b0 <> Script b1 = Script (liftM2 (<>) b0 b1)

{-
Could also be implemented with Control.Monad.Trans.State:
mappend = liftA2 mappend
-}
instance Monoid Script where
   mempty = Script $ return mempty
   mappend = (<>)

instance Semigroup Body where
   Body f0 c0 <> Body f1 c1 = Body (f0 <> f1) (c0 <> c1)

{-
Could also be implemented with Control.Monad.Trans.State:
mappend = liftA2 mappend
-}
instance Monoid Body where
   mempty = Body mempty mempty
   mappend = (<>)

class C gfx where
   toScript :: gfx -> Script
