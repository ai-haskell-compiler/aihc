{- |
Channel mode messages
-}
module Sound.MIDI.Message.Channel.Mode
    (T(..), get, put,
     fromControllerValue, toControllerValue, ) where

import           Sound.MIDI.Parser.Primitive
import qualified Sound.MIDI.Parser.Class as Parser

import qualified Sound.MIDI.Writer.Basic as Writer

import Sound.MIDI.Parser.Report (UserMessage, )

import qualified Control.Monad.Exception.Asynchronous as Async
import Data.Maybe.HT (toMaybe, )
import Control.Monad.Trans.Class (lift, )
import Control.Monad (liftM, )

import Test.QuickCheck (Arbitrary(arbitrary), )
import qualified Test.QuickCheck as QC



data T =
     AllSoundOff
   | ResetAllControllers
   | LocalControl Bool
   | AllNotesOff
   | OmniMode Bool
   | MonoMode Int
   | PolyMode
     deriving (Show, Eq, Ord)

instance Arbitrary T where
   arbitrary =
      QC.oneof $
         return AllSoundOff :
         return ResetAllControllers :
         liftM  LocalControl arbitrary :
         return AllNotesOff :
         liftM  OmniMode arbitrary :
         liftM  MonoMode (QC.choose (0,16)) :
         return PolyMode :
         []


-- * serialization

get :: Parser.C parser => Int -> Parser.Fragile parser T
get mode =
   do x <- get1
      lift $ Parser.warnIncomplete $ uncurry Async.Exceptional $ fromControllerValue (mode,x)

{- |
This function is also used in alsa-midi,
we could give it the result type @Parser.PossiblyIncomplete T@ otherwise.
-}
fromControllerValue :: (Show a, Integral a) => (a, a) -> (Maybe UserMessage, T)
fromControllerValue (mode,x) =
   case mode of
      0x78 ->
         (checkValidValue "AllSoundOff" [0] x,
          AllSoundOff)
      0x79 ->
         (checkValidValue "ResetAllControllers" [0] x,
          ResetAllControllers)
      0x7A ->
         (checkValidValue "LocalControl" [0,127] x,
          LocalControl (x/=0))
      0x7B ->
         (checkValidValue "AllNotesOff" [0] x,
          AllNotesOff)
      0x7C ->
         (checkValidValue "OmniMode Off" [0] x,
          OmniMode False)
      0x7D ->
         (checkValidValue "OmniMode On" [0] x,
          OmniMode True)
      0x7E ->
         (Nothing, MonoMode (fromIntegral x))
      0x7F ->
         (checkValidValue "PolyMode On" [0] x,
          PolyMode)
      _ -> error ("Channel.Mode.get: mode value out of range: " ++ show mode)


checkValidValue ::
   (Show a, Integral a) => String -> [a] -> a -> Maybe UserMessage
checkValidValue name validValues value =
   toMaybe
      (not (elem value validValues))
      ("Invalid value for " ++ name ++ ": " ++ show value)


put :: Writer.C writer => T -> writer
put mode =
   let (code, value) = toControllerValue mode
   in  Writer.putByteList [code, value]

toControllerValue :: Integral a => T -> (a, a)
toControllerValue mode =
   case mode of
      AllSoundOff         -> (,) 0x78 0
      ResetAllControllers -> (,) 0x79 0
      LocalControl b      -> (,) 0x7A (if b then 127 else 0)
      AllNotesOff         -> (,) 0x7B 0
      OmniMode b          -> (,) (if b then 0x7D else 0x7C) 0
      MonoMode x          -> (,) 0x7E (fromIntegral x)
      PolyMode            -> (,) 0x7F 0
