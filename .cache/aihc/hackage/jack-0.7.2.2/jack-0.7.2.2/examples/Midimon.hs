module Main where

import qualified Sound.JACK.MIDI as MIDI
import Sound.JACK (NFrames(NFrames), )

import qualified Sound.MIDI.Message as Msg


main :: IO ()
main = MIDI.main foo

foo :: NFrames -> (NFrames, Msg.T) -> IO (NFrames, Msg.T)
foo (NFrames cycleStart) (tf@(NFrames t), e) = do
    putStrLn $ "Time: " ++ show (cycleStart + t) ++ " " ++
       case e of
          Msg.Channel b -> "MidiMsg.Channel " ++ show b
          Msg.System  _ -> "MidiMsg.System ..."
    return (tf, e)

