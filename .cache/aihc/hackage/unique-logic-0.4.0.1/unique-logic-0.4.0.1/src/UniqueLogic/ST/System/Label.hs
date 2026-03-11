module UniqueLogic.ST.System.Label (
   -- * Preparation
   Variable,
   globalVariable,
   -- * Posing statements
   T,
   Sys.localVariable,
   Sys.constant,
   Sys.assignment2,
   Sys.assignment3,
   Sys.Apply, Sys.arg, Sys.runApply,
   -- * Solution
   Sys.solve,
   Sys.query,
   Sys.queryForbid,
   Sys.queryIgnore,
   Sys.queryVerify,
   ) where

import qualified UniqueLogic.ST.System as Sys
import qualified UniqueLogic.ST.MonadTrans as UMT
import qualified UniqueLogic.ST.Duplicate as Duplicate

import qualified Control.Monad.Trans.Writer as MW
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT, )
import Control.Monad.ST (ST, )

import Data.Monoid (Monoid, )
import Data.Traversable (traverse, )

import Prelude hiding (log, )


type T w = Sys.T (MW.WriterT w)
type Variable w = Sys.Variable (MW.WriterT w)

globalVariable ::
   (Monoid w, Duplicate.C a) =>
   (a -> MW.Writer w a) -> ST s (Variable w s a)
globalVariable log =
   Sys.globalVariable
      (\al av -> Sys.updateIfNew al av . wrap log)

wrap ::
   (Monoid w) =>
   (a -> MW.Writer w b) ->
   MaybeT (ST s) a -> MaybeT (UMT.Wrap (MW.WriterT w) (ST s)) b
wrap log =
   mapMaybeT $
      UMT.wrap . MW.WriterT . fmap (MW.runWriter . traverse log)
--      UMT.wrap . MW.writer . MW.runWriter . traverse log <=< UMT.lift
