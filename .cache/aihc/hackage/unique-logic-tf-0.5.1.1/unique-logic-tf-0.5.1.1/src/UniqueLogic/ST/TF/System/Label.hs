module UniqueLogic.ST.TF.System.Label (
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
   ) where

import qualified UniqueLogic.ST.TF.System as Sys
import qualified UniqueLogic.ST.TF.MonadTrans as UMT

import qualified Control.Monad.Trans.Writer as MW
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT, )
import Control.Monad (liftM, )

import qualified Data.Ref as Ref
import Data.Monoid (Monoid, )
import Data.Traversable (traverse, )

import Prelude hiding (log, )


type T w = Sys.T (MW.WriterT w)
type Variable w = Sys.Variable (MW.WriterT w)

globalVariable ::
   (Monoid w, Ref.C s) =>
   (a -> MW.Writer w a) -> s (Variable w s a)
globalVariable log =
   Sys.globalVariable
      (\al av -> Sys.updateIfNew al av . wrap log)

wrap ::
   (Monoid w, Ref.C s) =>
   (a -> MW.Writer w b) ->
   MaybeT s a -> MaybeT (UMT.Wrap (MW.WriterT w) s) b
wrap log =
   mapMaybeT $
      UMT.wrap . MW.WriterT . liftM (MW.runWriter . traverse log)
--      UMT.wrap . MW.writer . MW.runWriter . traverse log <=< UMT.lift
