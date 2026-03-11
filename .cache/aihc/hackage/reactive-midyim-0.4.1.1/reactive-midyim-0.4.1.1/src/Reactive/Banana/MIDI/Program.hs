module Reactive.Banana.MIDI.Program (
   Reactive.Banana.MIDI.Program.traverse, traverseSeek,
   next, seek, maybeNoteOn,
   asBanks,
   ) where

import qualified Sound.MIDI.Message.Class.Query as Query
import qualified Sound.MIDI.Message.Class.Construct as Construct

import Sound.MIDI.Message.Channel (Channel, )
import Sound.MIDI.Message.Channel.Voice (Program, fromProgram, toProgram, )

import qualified Control.Monad.Trans.State as MS

import qualified Data.Traversable as Trav
import Control.Monad (join, mplus, )
import Data.Tuple.HT (mapFst, mapSnd, )
import Data.Maybe.HT (toMaybe, )


next ::
   (Construct.C msg) =>
   Channel -> MS.State [Program] (Maybe msg)
next chan =
   MS.state $ \pgms ->
   case pgms of
      pgm:rest -> (Just $ Construct.program chan pgm, rest)
      [] -> (Nothing, [])

seek :: Int -> Program -> MS.State [Program] (Maybe msg)
seek maxSeek pgm =
   fmap (const Nothing) $
   MS.modify $
      uncurry (++) .
      mapFst (dropWhile (pgm/=)) .
      splitAt maxSeek


{-
Maybe we should use @Stream Program@ instead of @[Program]@.
-}
{- |
Before every note switch to another instrument
according to a list of programs given as state of the State monad.
I do not know how to handle multiple channels in a reasonable way.
Currently I just switch the instrument independent from the channel,
and send the program switch to the same channel as the beginning note.
-}
traverse ::
   (Query.C msg, Construct.C msg) =>
   msg -> MS.State [Program] (Maybe msg)
traverse =
   fmap join . Trav.traverse next . maybeNoteOn

{- |
This function extends 'traverse'.
It reacts on external program changes
by seeking an according program in the list.
This way we can reset the pointer into the instrument list.
However the search must be limited in order to prevent an infinite loop
if we receive a program that is not contained in the list.
-}
traverseSeek ::
   (Query.C msg, Construct.C msg) =>
   Int ->
   msg -> MS.State [Program] (Maybe msg)
traverseSeek maxSeek e =
   fmap join $ Trav.sequence $
   mplus
      (fmap next $ maybeNoteOn e)
      (fmap (seek maxSeek . snd) $ Query.program e)

maybeNoteOn :: (Query.C msg) => msg -> Maybe Channel
maybeNoteOn msg =
   Query.noteExplicitOff msg >>= \(c, (_p, _v, on)) -> toMaybe on c



{- |
> > replace [1,2,3,4] 5 [10,11,12,13]
> (True,[10,11,2,13])
-}
replace :: Real i => [i] -> i -> [i] -> (Bool, [i])
replace (n:ns) pgm pt =
   let (p,ps) =
          case pt of
             [] -> (0,[])
             (x:xs) -> (x,xs)
   in  if pgm<n
         then (True, pgm:ps)
         else mapSnd (p:) $
              replace ns (pgm-n) ps
replace [] _ ps = (False, ps)

fromBanks :: Real i => [i] -> [i] -> i
fromBanks ns ps =
   foldr (\(n,p) s -> p+n*s) 0 $
   zip ns ps

{- |
Interpret program changes as a kind of bank switches
in order to increase the range of instruments
that can be selected via a block of patch select buttons.

@asBanks ns@ divides the first @sum ns@ instruments
into sections of sizes @ns!!0, ns!!1, ...@.
Each program in those sections is interpreted as a bank in a hierarchy,
where the lower program numbers are the least significant banks.
Programs from @sum ns@ on are passed through as they are.
@product ns@ is the number of instruments
that you can address using this trick.
In order to avoid overflow it should be less than 128.

E.g. @asBanks [n,m]@ interprets subsequent program changes to
@a@ (@0<=a<n@) and @n+b@ (@0<=b<m@)
as a program change to @b*n+a@.
@asBanks [8,8]@ allows to select 64 instruments
by 16 program change buttons,
whereas @asBanks [8,4,4]@
allows to address the full range of MIDI 128 instruments
with the same number of buttons.
-}
asBanks ::
   (Query.C msg, Construct.C msg) =>
   [Int] ->
   msg -> MS.State [Int] msg
asBanks ns e =
   maybe
      (return e)
      (\(chan,pgm) -> do
          valid <- MS.state $ replace ns (fromProgram pgm)
          fmap (Construct.program chan) $
             if valid
               then MS.gets (toProgram . fromBanks ns)
               else return pgm) $
   Query.program e
