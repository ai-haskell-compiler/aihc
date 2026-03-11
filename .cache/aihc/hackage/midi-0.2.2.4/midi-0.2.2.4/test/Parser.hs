module Parser where

import qualified Sound.MIDI.Parser.Report as Report
import qualified Sound.MIDI.Parser.Class  as Parser
import qualified Sound.MIDI.Parser.Stream as StreamParser

import Control.Monad.Trans.Class (lift, )

import qualified Numeric.NonNegative.Wrapper as NonNeg


lazinessZeroOrMoreByteList :: NonNeg.Int -> Int -> Bool
lazinessZeroOrMoreByteList pos byte =
   let result =
          Report.result $
          StreamParser.runIncomplete (lift (Parser.zeroOrMore Parser.getByte)) $
          StreamParser.ByteList $ repeat $ fromIntegral byte
       char = show result !! mod (NonNeg.toNumber pos) 1000
   in  char == char
