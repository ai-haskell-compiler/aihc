module Sound.MIDI.Writer.Status (
   module Sound.MIDI.Writer.Status,
   lift,
   ) where

import Sound.MIDI.Parser.Status (Channel)

import qualified Data.Monoid.State       as State
import qualified Data.Monoid.Transformer as Trans
import Data.Monoid.Transformer (lift, )

import qualified Data.Monoid.HT as MonoidHT
import Data.Monoid (Monoid, mempty, mappend, mconcat, )
import Data.Semigroup (Semigroup, sconcat, (<>), )
import Sound.MIDI.Monoid (genAppend, genConcat, nonEmptyConcat, )


data Uncompressed = Uncompressed

newtype Compressed = Compressed Status
type Status = Maybe (Int,Channel)

{- |
'status' can be 'Uncompressed' for files ignoring the running status
or 'Compressed' for files respecting the running status.
-}
newtype T compress writer = Cons {decons :: State.T compress writer}


instance Semigroup writer => Semigroup (T compress writer) where
   Cons x <> Cons y = Cons $ x<>y
   sconcat = nonEmptyConcat Cons decons

instance Monoid writer => Monoid (T compress writer) where
   mempty = Cons $ mempty
   mappend = genAppend Cons decons
   mconcat = genConcat Cons decons


class Compression compress where
   {- |
   Given a writer that emits a status, generate a stateful writer,
   that decides whether to run the status emittor.
   -}
   change :: (Monoid writer) => (Int, Channel) -> writer -> T compress writer
   initState :: compress

instance Compression Uncompressed where
   change _ emit = Cons $ State.pure emit
   initState = Uncompressed

instance Compression Compressed where
   change x emit =
      Cons $
      State.Cons $ \(Compressed my) ->
         let mx = Just x
         in  (MonoidHT.when (mx/=my) emit, Compressed mx)
   initState = Compressed Nothing

clear :: (Compression compress, Monoid writer) => T compress writer
clear = Cons $ State.put initState


instance Trans.C (T compress) where
   lift = fromWriter

fromWriter :: (Monoid writer) => writer -> T compress writer
fromWriter = Cons . lift

toWriter :: (Compression compress, Monoid writer) => T compress writer -> writer
toWriter = State.evaluate initState . decons

toWriterWithStatus :: (Monoid writer) => T Compressed writer -> writer
toWriterWithStatus = toWriter

toWriterWithoutStatus :: (Monoid writer) => T Uncompressed writer -> writer
toWriterWithoutStatus = toWriter
