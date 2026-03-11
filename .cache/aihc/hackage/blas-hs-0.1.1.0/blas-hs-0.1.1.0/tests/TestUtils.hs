module TestUtils where
import Control.Applicative (Applicative((<*>), pure))
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

data TestState
  = TestState
    { quiet     :: Bool     -- ^ Whether success messages should be suppressed
    , failCount :: Int
    }

initTestState :: TestState
initTestState
  = TestState
    { quiet     = False
    , failCount = 0
    }

newtype Test a = Test { runTestM :: TestState -> IO (a, TestState) }

instance Functor Test where
  fmap f u = return f <*> u

instance Applicative Test where
  pure      = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return (f x)

instance Monad Test where
  return x     = Test $ \ s -> return (x, s)
  Test f >>= u = Test $ \ s -> do
    (x, s') <- f s
    runTestM (u x) s'
  fail msg     = do
    failTest "aborted"
    liftIO (fail msg)

runTest :: Test () -> IO ()
runTest test = do
  ((), s) <- runTestM test initTestState
  if failCount s > 0 then exitFailure else exitSuccess

passTest :: String -> Test ()
passTest msg = do
  s <- get
  if quiet s
    then return ()
    else liftIO (putStrLn msg')
  where msg' = "ok" ++ if null msg then "" else printf " (%s)" msg

failTest :: String -> Test ()
failTest msg = do
  liftIO (hPutStrLn stderr ("*** FAIL: " ++ msg))
  modify $ \ state -> state { failCount = succ (failCount state) }

-- | The test passes only if the boolean argument is true.
expect :: String -> Bool -> Test ()
expect msg False = failTest msg
expect msg True  = passTest msg

liftIO :: IO a -> Test a
liftIO m = Test $ \ s -> do
  result <- m
  return (result, s)

get :: Test TestState
get = Test $ \ s -> return (s, s)

put :: TestState -> Test ()
put s = Test $ \ _ -> return ((), s)

modify :: (TestState -> TestState) -> Test ()
modify f = do
  s <- get
  put (f s)
