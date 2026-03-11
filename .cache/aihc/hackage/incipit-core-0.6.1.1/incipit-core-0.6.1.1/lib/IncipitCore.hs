{-# language CPP #-}
-- |This is the central module on which to build upon when constructing Preludes for Polysemy libraries.
-- It reexports most core effects.
module IncipitCore (
  module Incipit.Exception,
  module IncipitBase,
  module Polysemy,
  module Polysemy.Async,
  module Polysemy.AtomicState,
  module Polysemy.Error,
  module Polysemy.Fail,
  module Polysemy.Input,
  module Polysemy.Output,
  module Polysemy.Reader,
  module Polysemy.Resource,
#if MIN_VERSION_polysemy(1,8,0)
  module Polysemy.Scoped,
#endif
  module Polysemy.State,
  module Polysemy.Tagged,
  module Polysemy.Writer,
  module IncipitCore,
#if !MIN_VERSION_polysemy(1,8,0)
  send,
#endif
) where

import Incipit.Exception
import IncipitBase
import Polysemy hiding (run)
import Polysemy.Async (Async)
import Polysemy.Async hiding (Async, Await, Cancel)
import Polysemy.AtomicState (AtomicState)
import Polysemy.AtomicState hiding (AtomicGet, AtomicState)
import Polysemy.Error hiding (Catch, Throw)
import Polysemy.Fail (Fail)
import Polysemy.Fail hiding (Fail)
import Polysemy.Input (Input)
import Polysemy.Input hiding (Input)
#if !MIN_VERSION_polysemy(1,8,0)
import Polysemy.Internal (send)
#endif
import Polysemy.Internal.Kind (Append)
import Polysemy.Output (Output)
import Polysemy.Output hiding (Output)
import Polysemy.Reader hiding (Ask, Local)
import Polysemy.Resource hiding (Bracket, BracketOnError)
#if MIN_VERSION_polysemy(1,8,0)
import Polysemy.Scoped
#endif
import Polysemy.State hiding (Get, Put)
import Polysemy.Tagged (Tagged)
import Polysemy.Tagged hiding (Tagged)
import Polysemy.Writer hiding (Listen, Pass, Tell)

-- |Convenience type alias for concatenating two effect rows.
type a ++ b =
  Append a b

infixr 5 ++

-- |Convenience alias for @pureT ()@.
unitT ::
  Functor f =>
  Sem (WithTactics e f m r) (f ())
unitT =
  pureT ()
{-# inline unitT #-}
