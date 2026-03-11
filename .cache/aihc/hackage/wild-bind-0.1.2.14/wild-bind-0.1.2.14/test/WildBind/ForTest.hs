module WildBind.ForTest
    ( SampleInput (..)
    , SampleState (..)
    , SampleBackState (..)
    , inputAll
    , execAll
    , evalStateEmpty
    , boundDescs
    , boundDescs'
    , curBoundInputs
    , curBoundDescs
    , curBoundDesc
    , checkBoundInputs
    , checkBoundDescs
    , checkBoundDesc
    , withRefChecker
    ) where

import           Control.Applicative       (pure, (<$>), (<*>))
import           Control.Monad             (join)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Control.Monad.Trans.State as State
import           Data.IORef                (IORef, newIORef, readIORef)
import           Data.Monoid               (mempty)
import           Test.Hspec                (shouldBe, shouldMatchList, shouldReturn)
import           Test.QuickCheck           (Arbitrary (arbitrary, shrink), arbitraryBoundedEnum)

import qualified WildBind.Binding          as WB
import qualified WildBind.Description      as WBD

data SampleInput = SIa | SIb | SIc deriving (Bounded, Enum, Eq, Ord, Show)

instance Arbitrary SampleInput where
  arbitrary = arbitraryBoundedEnum

data SampleState
  = SS
      { unSS :: String
      }
  deriving (Eq, Ord, Show)

instance Arbitrary SampleState where
  arbitrary = SS <$> arbitrary
  shrink (SS s) = SS <$> shrink s


data SampleBackState
  = SB
      { unSB :: Int
      }
  deriving (Eq, Ord, Show)

instance Enum SampleBackState where
  toEnum = SB
  fromEnum = unSB


inputAll :: Ord i => WB.Binding s i -> s -> [i] -> IO (WB.Binding s i)
inputAll b _ [] = return b
inputAll binding state (i:rest) = case WB.boundAction binding state i of
  Nothing  -> inputAll binding state rest
  Just act -> join $ inputAll <$> WB.actDo act <*> return state <*> return rest

execAll :: Ord i => s -> [i] -> State.StateT (WB.Binding s i) IO ()
execAll state inputs = do
  b <- State.get
  next_b <- liftIO $ inputAll b state inputs
  State.put next_b

evalStateEmpty :: State.StateT (WB.Binding SampleState SampleInput) IO () -> IO ()
evalStateEmpty s = State.evalStateT s mempty

toDesc :: (i, WB.Action m a) -> (i, WBD.ActionDescription)
toDesc (i, act) = (i, WB.actDescription act)

boundDescs :: WB.Binding s i -> s -> [(i, WBD.ActionDescription)]
boundDescs b s = map toDesc $ WB.boundActions b s

boundDescs' :: WB.Binding' bs fs i -> bs -> fs -> [(i, WBD.ActionDescription)]
boundDescs' b bs fs = map toDesc $ WB.boundActions' b bs fs

curBoundInputs :: s -> State.StateT (WB.Binding s i) IO [i]
curBoundInputs s = State.gets WB.boundInputs <*> pure s

curBoundDescs :: s -> State.StateT (WB.Binding s i) IO [(i, WBD.ActionDescription)]
curBoundDescs s = State.gets boundDescs <*> pure s

curBoundDesc :: Ord i => s -> i -> State.StateT (WB.Binding s i) IO (Maybe WBD.ActionDescription)
curBoundDesc s i = (fmap . fmap) WB.actDescription $ State.gets WB.boundAction <*> pure s <*> pure i

checkBoundInputs :: (Eq i, Show i) => s -> [i] -> State.StateT (WB.Binding s i) IO ()
checkBoundInputs fs expected = liftIO . (`shouldMatchList` expected) =<< curBoundInputs fs

checkBoundDescs :: (Eq i, Show i) => s -> [(i, WBD.ActionDescription)] -> State.StateT (WB.Binding s i) IO ()
checkBoundDescs fs expected = liftIO . (`shouldMatchList` expected) =<< curBoundDescs fs

checkBoundDesc :: (Ord i) => s -> i -> WBD.ActionDescription -> State.StateT (WB.Binding s i) IO ()
checkBoundDesc fs input expected = liftIO . (`shouldBe` Just expected) =<< curBoundDesc fs input

withRefChecker :: (Eq a, Show a, MonadIO m)
               => a
               -> (IORef a -> (a -> m ()) -> m ())
               -> m ()
withRefChecker init_ref action = do
  out <- liftIO $ newIORef init_ref
  let checkOut expected = liftIO $ readIORef out `shouldReturn` expected
  action out checkOut
