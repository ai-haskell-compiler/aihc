module Text.HTML.Tagchup.Parser.Core (
   T,
   nextChar, withDefault, withDefault',
   run, gets, tell, censor, mfix,
   allowFail, allowFail', allowEmit,
   module Data.Functor.Identity,
   ) where


import qualified Text.HTML.Tagchup.Parser.Status as Status
import qualified Text.HTML.Tagchup.Parser.Stream as Stream
import Data.Tuple.HT (mapSnd, )

import qualified Control.Monad.Trans.State as State

import Control.Monad.Trans.Writer (WriterT(..), mapWriterT, tell, censor, )
import Control.Monad.Trans.State (StateT(..), mapStateT, )
import Control.Monad.Fix (mfix)
import Data.Functor.Identity (Identity(..), )
import Control.Monad.Trans.Class (lift, )
import Control.Monad (liftM, )

import Data.Monoid (Monoid, mempty, )


{-
We need to declare whether parts of the parser
can or cannot fail and can or cannot emit something through the writer.
This way we can get rid of the former functions 'force' and 'ignoreEmit'
which generate errors at run-time
if the assumptions of non-failing or non-emission were wrong.
Now, since we declare this properties in types,
runtime errors cannot happen.

The downside is that we cannot easily extend that scheme
to embedded monads that are sources of Chars.
@StateT s Maybe@ and @MaybeT (State s)@ significantly differ
in case of parser failures.
The first one "resets" its state
(more precisely, you would use 'mplus' to give alternative parsers a try,
and 'mplus' would keep the original state),
and the second one stays with the updated state.
The second one would be close to @MaybeT (ReaderT Handle IO)@
which would allow to use @hGetChar@ as character source,
but for IO functions we had to maintain a list of characters
that might have to be re-parsed by a parser alternative.
-}
type T input output fail = WriterT output (StateT (Status.T input) fail)


run :: Monad fail =>
   T input output fail a -> Status.T input -> fail (a, Status.T input, output)
run p =
   liftM (\((a,w),st) -> (a,st,w)) . runStateT (runWriterT p)


nextChar :: (Monoid output, Stream.C input) =>
   T input output Maybe Char
nextChar =
   lift $ Stream.getChar


gets :: (Monoid output, Monad fail) =>
   (Status.T input -> a) -> T input output fail a
gets = lift . State.gets

-- this replaces 'ignoreEmit'
allowEmit ::
   (Monad fail, Monoid output) =>
   T input () fail a -> T input output fail a
allowEmit =
   mapWriterT (liftM (mapSnd (const mempty)))

allowFail' ::
   StateT s Identity a -> StateT s Maybe a
allowFail' =
   mapStateT (Just . runIdentity)

-- this replaces 'force'
allowFail :: T input output Identity a -> T input output Maybe a
allowFail =
   mapWriterT allowFail'

withDefault' ::
   StateT s Maybe a ->
   StateT s Identity a ->
   StateT s Identity a
withDefault' p q =
   StateT $ \st ->
      maybe (runStateT q st) Identity (runStateT p st)

withDefault ::
   T input output Maybe a ->
   T input output Identity a ->
   T input output Identity a
withDefault p q =
   WriterT $ withDefault' (runWriterT p) (runWriterT q)
