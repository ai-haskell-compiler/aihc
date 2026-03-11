{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections      #-}
module Test.Tasty.Ingredients.FailFast
    ( failFast
    , FailFast(..)
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Data.IntMap.Strict     as IM
import           Data.Monoid
import           Data.Proxy
import           Data.Typeable
import           Test.Tasty.Ingredients
import           Test.Tasty.Options
import           Test.Tasty.Runners
-------------------------------------------------------------------------------
import           Prelude
-------------------------------------------------------------------------------


-- | Decorate a TestReporter. Only applicable to TestReporters. Will
-- be a noop for TestManager.
failFast :: Ingredient -> Ingredient
failFast (TestReporter opts f) = TestReporter (ffOpt:opts) f'
  where ffOpt = Option (Proxy :: Proxy FailFast)
        f' oset tree = let FailFast ff = lookupOption oset
                       in if ff
                            then ffHijack <$> f oset tree
                            else f oset tree
failFast i = i -- not applicable


-------------------------------------------------------------------------------
ffHijack :: (StatusMap -> IO (Time -> IO Bool)) -> StatusMap -> IO (Time -> IO Bool)
ffHijack f sm = do
  _ <- forkIO (work sm)
  f sm

-------------------------------------------------------------------------------
newtype FailFast = FailFast Bool deriving (Show, Eq, Typeable)

instance IsOption FailFast where
  defaultValue = FailFast False
  parseValue = fmap FailFast . safeRead
  optionName = return "fail-fast"
  optionHelp = return "Fail the whole suite when the first test fails."
  optionCLParser = flagCLParser Nothing (FailFast True)


-------------------------------------------------------------------------------
work :: StatusMap -> IO ()
work sm = atomically $ do
  check =<< anyFailed sm
  failAll sm


-------------------------------------------------------------------------------
anyFailed :: StatusMap -> STM Bool
anyFailed = anyM (fmap isFailed . readTVar) . IM.elems
  where isFailed (Done (Result { resultOutcome = Failure _})) = True
        isFailed _                                            = False


-------------------------------------------------------------------------------
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = return False
anyM p (x:xs)   = do q <- p x
                     if q
                       then return True
                       else anyM p xs
-------------------------------------------------------------------------------
failAll :: StatusMap -> STM ()
failAll = mapM_ failOne . IM.elems


-------------------------------------------------------------------------------
failOne :: TVar Status -> STM ()
failOne = flip modifyTVar' go
  where go NotStarted = Done res
        go x = x
        res = Result { resultOutcome = Failure TestFailed
                     , resultDescription = mempty
#if MIN_VERSION_tasty(0,11,0)
                     , resultShortDescription = mempty
#endif
                     , resultTime = 0}
