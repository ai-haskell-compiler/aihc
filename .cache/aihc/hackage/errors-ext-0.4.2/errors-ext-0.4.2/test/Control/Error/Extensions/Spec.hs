--
-- Copyright 2017, 2018 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

module Control.Error.Extensions.Spec
  ( tests
  ) where

import Test.HUnit.Base hiding (Label)
import Control.Error.Extensions
import Control.Exception
import Control.Monad.Error.Class
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString as S (ByteString)
import Data.Conduit hiding (ConduitM)
import qualified Data.Conduit.Combinators as N
import Data.Conduit.Lift
import Data.Conduit.Parsers.Binary.Get
import Data.Conduit.Parsers.Binary.Put
import Data.IORef
import System.IO.Error

tests :: Test
tests = TestList
  $  ((TestCase . runBracketTest) <$> bracketTests)
  ++ [TestCase testPrevNaturalsFile]

data Resource = Resource { acquired :: !Bool, released :: !Bool, processed :: !Bool } deriving (Eq, Show)

resource :: IO (IORef Resource)
resource = newIORef $ Resource False False False

type Acquire = IORef Resource -> ExceptT String IO (IORef Resource)

acquireOk :: Acquire
acquireOk r = do
  lift $ modifyIORef' r $ \x -> x { acquired = True }
  return r

acquireEx :: Acquire
acquireEx _ = lift $ fail "acquireEx"

acquireEr :: Acquire
acquireEr _ = throwE "acquireEr"

type Release = IORef Resource -> ExceptT String IO ()

releaseOk :: Release
releaseOk r = lift $ modifyIORef' r $ \x -> x { released = True }

releaseEx :: Release
releaseEx _ = lift $ fail "releaseEx"

releaseEr :: Release
releaseEr _ = throwE "releaseEr"

type Process = IORef Resource -> ExceptT String IO Resource

processOk :: Process
processOk r = lift $ do
  v <- readIORef r
  modifyIORef' r $ \x -> x { processed = True }
  return v

processEx :: Process
processEx _ = lift $ fail "processEx"

processEr :: Process
processEr _ = throwE "processEr"

data Err = Err { isException :: !Bool, message :: !String } deriving (Eq, Show)

bracketTest :: Acquire -> Release -> Process -> IO (Resource, Either Err Resource)
bracketTest acquire release process = do
  r <- resource
  p <- catch (either (\x -> Left $ Err False x) Right <$> (runExceptT $ bracketE (acquire r) release process)) $ \e ->
    let _ = e :: IOError in return $ Left $ Err True $ ioeGetErrorString e
  f <- readIORef r
  return (f, p)

data TestData = TestData String !Acquire !Release !Process !(Resource, Either Err Resource)

bracketTests :: [TestData]
bracketTests =
  [ TestData "1" acquireOk releaseOk processOk (Resource True True True, Right $ Resource True False False)
  , TestData "2" acquireEx releaseOk processOk (Resource False False False, Left $ Err { isException = True, message = "acquireEx" })
  , TestData "3" acquireEr releaseOk processOk (Resource False False False, Left $ Err { isException = False, message = "acquireEr" })
  , TestData "4" acquireOk releaseEx processOk (Resource True False True, Left $ Err { isException = True, message = "releaseEx" })
  , TestData "5" acquireOk releaseEr processOk (Resource True False True, Left $ Err { isException = False, message = "releaseEr" })
  , TestData "6" acquireOk releaseOk processEx (Resource True True False, Left $ Err { isException = True, message = "processEx" })
  , TestData "7" acquireOk releaseOk processEr (Resource True True False, Left $ Err { isException = False, message = "processEr" })
  , TestData "8" acquireOk releaseEx processEx (Resource True False False, Left $ Err { isException = True, message = "processEx" })
  , TestData "9" acquireOk releaseEr processEx (Resource True False False, Left $ Err { isException = True, message = "processEx" })
  , TestData "10" acquireOk releaseEx processEr (Resource True False False, Left $ Err { isException = False, message = "processEr" })
  , TestData "11" acquireOk releaseEr processEr (Resource True False False, Left $ Err { isException = False, message = "processEr" })
  ]

runBracketTest :: TestData -> Assertion
runBracketTest (TestData m a r p e) = do
  t <- bracketTest a r p
  assertEqual m e t

data PrevNaturalError = NotNatural Integer | Minimal deriving (Eq, Show)

prevNatural :: (Monad m, Integral i) => ConduitT i i (ExceptT PrevNaturalError m) ()
prevNatural = awaitForever $ \ !n -> do
  if n <= 0 then throwError (NotNatural $ fromIntegral n) else return ()
  if n == 1 then throwError Minimal else return ()
  yield $ n - 1

prevNaturalsFile :: Monad m => ConduitT S.ByteString S.ByteString (ExceptT PrevNaturalError m) ()
prevNaturalsFile = (eitherVoidR <$> runGet (iterateM_ (const $ yield =<< getInt8) (error ""))) .| prevNatural .| awaitForever (runPut . putInt8)

testPrevNaturalsFile :: Assertion
testPrevNaturalsFile = do
  assertEqual "" (Left Minimal, "") $ runConduitPure $ yield "\x01\x02\x03" .| runExceptC prevNaturalsFile `fuseBoth` N.sinkLazy
  assertEqual "" (Right (), "\x02\x07\x08") $ runConduitPure $ yield "\x03\x08\x09" .| runExceptC prevNaturalsFile `fuseBoth` N.sinkLazy
