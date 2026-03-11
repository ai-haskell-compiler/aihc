{- |
This module calls the 'soxi' command
which is available since 'sox' version 14.

We have to call 'soxi' for every option.
However we hide this in our interface,
such that we could do more efficiently,
if 'soxi' supports multiple outputs in future.
-}
module Sound.Sox.Information (
   T(Cons),
   simple,
   format,
   sampleRate,
   numberOfChannels,
   length,
   bitsPerSample,
   get,
   exampleMulti,
   exampleSingle,
   ) where

import qualified Control.Monad.Trans.Reader as MR
import Control.Applicative (Applicative, pure, liftA3, (<*>), )
import Data.String.HT (trim, )
import Text.Read.HT (maybeRead, )

import qualified System.Process as Proc
import qualified System.IO as IO
import Control.Exception (bracket, )
-- import System.IO.Error (ioError, userError, )
-- import System.Exit (ExitCode, )

import Prelude hiding (length, )


newtype T a =
   Cons (MR.ReaderT FilePath IO a)

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
   Cons $ MR.ReaderT $ \fileName ->
      bracket
         (Proc.runInteractiveProcess "soxi"
             (option : fileName : [])
             Nothing Nothing)
         (\(input,output,err,proc) ->
             mapM_ IO.hClose [input, output, err] >>
             Proc.terminateProcess proc)
         (\(_,output,_,_) ->
            maybe
               (ioError (userError "soxi returned rubbish"))
               return .
            rd =<<
            IO.hGetContents output)

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
get (Cons act) = MR.runReaderT act

exampleMulti :: IO (String, Int, Int)
exampleMulti =
   get (liftA3 (,,) format sampleRate bitsPerSample) "test.aiff"

exampleSingle :: IO Int
exampleSingle =
   get sampleRate "test.aiff"
