{- |
This module calls the 'soxi' command
which is available since 'sox' version 14.

This module is very clever as it calls 'soxi' once
with one option per requested piece of information
and parses the results in a correctly typed tuple.
Unfortunately, 'soxi' does not work this way.
It accepts only one option per call.
-}
module Sound.Sox.Private.Information where

import Control.Monad.Trans.Writer (Writer, writer, runWriter, )
import Control.Monad.Trans.State (StateT(StateT), runStateT, )
import Control.Monad.Trans.Class (lift, )
import Control.Applicative (Applicative, pure, liftA3, (<*>), )
import Data.Functor.Compose (Compose(Compose), )
import Data.List.HT (viewL, )
import Data.String.HT (trim, )
import Text.Read.HT (maybeRead, )

import qualified System.Process as Proc
import qualified System.IO as IO
import Control.Exception (bracket, )
-- import System.IO.Error (ioError, userError, )
-- import System.Exit (ExitCode, )

import Prelude hiding (length, )


{-
Cf. Synthesizer.Basic.Interpolation.PrefixReader.
-}
newtype T a =
   Cons (Compose (Writer [String]) (StateT [String] Maybe) a)

instance Functor T where
   {-# INLINE fmap #-}
   fmap f (Cons m) = Cons (fmap f m)

instance Applicative T where
   {-# INLINE pure #-}
   {-# INLINE (<*>) #-}
   pure = Cons . pure
   (Cons f) <*> (Cons x) = Cons (f <*> x)


simple :: Read a => (String -> Maybe a) -> String -> T a
simple rd option =
   Cons $ Compose $ writer
      (lift . rd =<< StateT viewL, [option])

format :: T String
format = simple Just "-t"

simpleRead :: String -> T Int
simpleRead = simple (maybeRead . trim)

sampleRate :: T Int
sampleRate = simpleRead "-r"

numberOfChannels :: T Int
numberOfChannels = simpleRead "-c"

length :: T Int
length = simpleRead "-s"

bitsPerSample :: T Int
bitsPerSample = simpleRead "-b"



get :: T a -> FilePath -> IO a
get (Cons (Compose w)) fileName =
   let (parser, opts) = runWriter w
   in  bracket
          (Proc.runInteractiveProcess "soxi"
              (opts ++ fileName : [])
              Nothing Nothing)
          (\(input,output,err,proc) ->
              mapM_ IO.hClose [input, output, err] >>
              Proc.terminateProcess proc)
          (\(_,output,_,_) ->
             maybe
                (ioError (userError "soxi returned rubbish"))
                (\(x,str) ->
                    if null str
                      then return x
                      else ioError (userError "soxi returned more lines than expected")) .
             runStateT parser . lines =<<
             IO.hGetContents output)

exampleMulti :: IO (String, Int, Int)
exampleMulti =
   get (liftA3 (,,) format sampleRate bitsPerSample) "test.aiff"

exampleSingle :: IO Int
exampleSingle =
   get sampleRate "test.aiff"
