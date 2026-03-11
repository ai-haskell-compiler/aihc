{- |
Module      :  $Header$
Description :  Simple Example of using the io-manager library
Copyright   :  (c) Mihai Maruseac
License     :  BSD3

Maintainer  :  mihai.maruseac@gmail.com
Stability   :  stable
Portability :  portable

A simple test module for IOManager which will echo the contents of the
input files to the output files, paired as specified in standard input:
each line of stdin is made of two words: «input output» meaning that the
contents of «input» should be echoed to «output». If input is «@stdin» then
the content to be echoed is from the standard input. If output is «@stdout»
then content should be written to standard ouput instead of a file. Same is
true for the case where «@stderr» is in the ouput part.

Assume that the stdin description is valid: no «@stdout»/«@stderr» on the
input part and no «@stdin» on the output part. Also, no file appears both
in the input and output part.
-}
module Main where

import Training.MM.IOManager

-- | The main function simply tells which is the function implemented by the
-- student.
main :: IO ()
main = wrapIO solve

-- | Solution of the problem. Add here your implementation.
solve :: Input -> Output -> Output
solve i = convert (map words . lines . getStdIn $ i) i

-- | Convert input representation to output representation.
convert :: [[String]] -> Input -> Output -> Output
convert [] _ o = o
convert ([fi, fo]:fs) i o = convert fs i $ link fi fo i o
convert _ _ _ = error "Invalid input"

-- | Link input and output and stream the contents between the two.
link :: String -> String -> Input -> Output -> Output
link "@stdin" fo i o = write fo (getStdIn i) o
link fi fo i o = write fo (getInputFile i fi) o

-- | Output the contents.
write :: String -> String -> Output -> Output
write "@stdout" text o = writeStdOut o text
write "@stderr" text o = writeStdErr o text
write fi text o = writeOutputFile o fi text
