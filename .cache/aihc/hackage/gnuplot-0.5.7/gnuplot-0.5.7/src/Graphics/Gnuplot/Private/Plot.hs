module Graphics.Gnuplot.Private.Plot where

import qualified Graphics.Gnuplot.Private.Display as Display
import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Graphics.Gnuplot.Private.File as FileClass
import qualified Graphics.Gnuplot.File as File
import Graphics.Gnuplot.Utility (quote, commaConcat, )

import qualified Control.Monad.Trans.Reader as MR
import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Class as MT
import qualified Data.Accessor.Monad.Trans.State as AccState
import qualified Data.Accessor.Tuple as AccTuple
import qualified Data.Foldable as Fold
import Control.Monad (liftM2, return, )
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup, (<>), )
import Data.Maybe (Maybe(Just, Nothing), mapMaybe, )
import Data.List (concatMap, map, (++), )
import Data.Function (($), (.), )


import qualified System.FilePath as Path
import System.FilePath (FilePath, (</>), )

import Prelude (Functor, fmap, String, show, Int, succ, writeFile, )


{- |
Plots can be assembled using 'mappend' or 'mconcat'
or several functions from "Data.Foldable".
-}
newtype T graph = Cons (MS.StateT Int (MR.Reader FilePath) [File graph])

pure :: [File graph] -> T graph
pure = Cons . return

instance Semigroup (T graph) where
   Cons s0 <> Cons s1 = Cons (liftM2 (<>) s0 s1)

{-
Could also be implemented with Control.Monad.Trans.State:
mappend = liftA2 mappend
-}
instance Monoid (T graph) where
   mempty = Cons $ return mempty
   mappend = (<>)


withUniqueFile :: String -> [graph] -> T graph
withUniqueFile content graphs = Cons $ do
   n <- MS.get
   dir <- MT.lift MR.ask
   MS.put $ succ n
   return $
      [File
         (dir </> Path.addExtension (tmpFileStem ++ show n) "csv")
         (Just content) graphs]

fromGraphs :: FilePath -> [graph] -> T graph
fromGraphs name gs =
   pure [File name Nothing gs]


data File graph =
   File {
      filename_ :: FilePath,
      content_ :: Maybe String,
      graphs_ :: [graph]
   }

instance FileClass.C (File graph) where
   write (File fn cont _) =
      Fold.mapM_ (writeFile fn) cont


tmpFileStem :: FilePath
tmpFileStem = "curve"

{-
tmpFile :: FilePath
tmpFile = tmpFileStem ++ ".csv"
-}


instance Functor T where
   fmap f (Cons mp) =
      Cons $
      fmap (map (\file -> file{graphs_ = map f $ graphs_ file}))
      mp

{- |
In contrast to the Display.toScript method instantiation
this function leaves the options,
and thus can be used to write the Display.toScript instance for Frame.
-}
toScript :: Graph.C graph => T graph -> Display.Script
toScript p@(Cons mp) =
   Display.Script $ do
      blocks <- AccState.liftT AccTuple.first mp
      let files =
             mapMaybe
                (\blk -> fmap (File.Cons (filename_ blk)) (content_ blk))
                blocks
          graphs =
             concatMap
                (\blk ->
                   map
                      (\gr ->
                         quote (filename_ blk) ++ " " ++
                         Graph.toString gr) $ graphs_ blk) $
             blocks
      return $
         Display.Body files
            [Graph.commandString (plotCmd p) ++ " " ++ commaConcat graphs]

optionsToScript :: Graph.C graph => OptionSet.T graph -> Display.Script
optionsToScript opts =
   Display.Script $
   AccState.liftT AccTuple.second $ do
      opts0 <- MS.get
      let opts1 = OptionSet.decons opts
      MS.put opts1
      return $
         Display.Body [] $
         OptionSet.diffToString opts0 opts1

defltOpts :: Graph.C graph => T graph -> OptionSet.T graph
defltOpts _ = Graph.defltOptions

instance Graph.C graph => Display.C (T graph) where
   toScript plot =
      optionsToScript (defltOpts plot)  `mappend`  toScript plot

plotCmd :: Graph.C graph => T graph -> Graph.Command graph
plotCmd _plot = Graph.command
