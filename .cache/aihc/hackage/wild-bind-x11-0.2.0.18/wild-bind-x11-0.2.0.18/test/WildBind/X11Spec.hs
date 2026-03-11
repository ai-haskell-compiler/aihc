{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module WildBind.X11Spec
    ( main
    , spec
    ) where

import           Control.Applicative       ((<$>))
import           Control.Concurrent.Async  (async, waitCatch)
import           Control.Exception         (Exception (fromException), finally, throwIO)
import           Control.Monad             (forM_)
import           Control.Monad.IO.Class    (liftIO)
import qualified Control.Monad.Trans.State as State
import           Data.IORef                (IORef, modifyIORef, newIORef, readIORef)
import           Data.List                 (intercalate, reverse)
import           Data.Monoid               ((<>))
import           Data.Text                 (unpack)
import           Data.Time.Clock           (diffUTCTime, getCurrentTime)
import qualified Graphics.X11.Xlib         as Xlib
import           System.IO                 (hPutStrLn, stderr)
import           Test.Hspec

import           WildBind                  (ActionDescription, Describable,
                                            FrontEnd (frontNextEvent, frontSetGrab, frontUnsetGrab),
                                            FrontEvent (FEChange, FEInput), Option (..), as, binds',
                                            defOption, justBefore, on, revise, run, startFrom,
                                            whenBack, wildBind')
import qualified WildBind.Description      as WBD
import qualified WildBind.Input.NumPad     as NumPad
import           WildBind.X11              (ActiveWindow, KeyEventType (..), XKeyEvent (..),
                                            XKeyInput, XMod (..), alt, ctrl, press, release, shift,
                                            super, withFrontEnd)
import qualified WildBind.X11.KeySym       as WKS

import           WildBind.X11.TestUtil     (checkIfX11Available, withGrabs)

newtype MyException
  = MyException String
  deriving (Eq, Ord, Show)

instance Exception MyException

main :: IO ()
main = hspec spec

maybeRun :: Expectation -> Expectation
#ifdef TEST_INTERACTIVE
maybeRun = id
#else
maybeRun _ = pendingWith ("You need to set test-interactive flag to run the test.")
#endif

p :: String -> IO ()
p = hPutStrLn stderr . ("--- " ++)

withFrontEndForTest :: (XKeyInput i, Describable i, Ord i) => (FrontEnd ActiveWindow i -> IO a) -> IO a
withFrontEndForTest action = withFrontEnd $ \front -> do
  _ <- frontNextEvent front -- discard the first FEChange event.
  action front

grabExp :: (Bounded i, Enum i, Show i, Eq i)
        => FrontEnd ActiveWindow i -> i -> Expectation
grabExp front grab_input = grabExpMain `finally` releaseAll where
  grabExpMain = do
    frontSetGrab front grab_input
    p ("Press some numpad keys (grab="++ show grab_input ++")..")
    ev <- frontNextEvent front
    p ("Got event: " ++ show ev)
    ev `shouldBe` FEInput grab_input
  releaseAll = mapM_ (frontUnsetGrab front) (enumFromTo minBound maxBound)

grabCase :: (Bounded i, Enum i, Show i, Eq i) => FrontEnd ActiveWindow i -> Expectation
grabCase front = mapM_ (grabExp front) (enumFromTo minBound maxBound)

stopWatchMsec :: IO a -> IO (a, Int)
stopWatchMsec act = do
  start <- getCurrentTime
  ret <- act
  end <- getCurrentTime
  return (ret, floor ((diffUTCTime end start) * 1000))

describeStr :: Describable a => a -> String
describeStr = unpack . WBD.describe

unshiftNewBinding :: Eq i => IORef [[(i,ActionDescription)]] -> [(i,ActionDescription)] -> IO ()
unshiftNewBinding ref got = do
  cur <- readIORef ref
  case cur of
   [] -> update
   (latest : _) -> if latest /= got
                   then update
                   else return ()
  where
    update = modifyIORef ref (got :)

spec :: Spec
spec = checkIfX11Available $ do
  describe "X11Front" $ do
    it "should first emit FEChange event when initialized" $ withFrontEnd $ \f -> do
      p "try to get the first event..."
      (ev, time) <- stopWatchMsec $ frontNextEvent f :: IO (FrontEvent ActiveWindow NumPad.NumPadUnlocked, Int)
      time `shouldSatisfy` (< 500)
      case ev of
        FEChange _ -> return ()
        _          -> expectationFailure ("FEChange is expected, but got " ++ show ev)
    it "should NOT throw exception when it tries to double-grab in the same process" $ withFrontEnd $ \f1 ->
      withFrontEnd $ \f2 -> do
        frontSetGrab f1 NumPad.NumLeft `shouldReturn` ()
        frontSetGrab f2 NumPad.NumLeft `shouldReturn` ()
    it "should control key grab based on ('release' || 'press')" $ maybeRun $ withFrontEnd $ \f -> do
      got_binds_rev <- newIORef []
      let opt = defOption { optBindingHook = unshiftNewBinding got_binds_rev,
                            optCatch = (\_ _ e -> throwIO e)
                          }
          b_base = binds' $ do
            on (press $ ctrl $ WKS.xK_g) `as` "P(C-g)" `run` liftIO (throwIO $ MyException "NG")
            on (release $ alt $ ctrl $ WKS.xK_x) `as` "R(M-C-x)" `run` liftIO (throwIO $ MyException "OK")
          b_0 = whenBack (== 0) $ binds' $ do
            on (press $ ctrl $ WKS.xK_i) `as` "P(C-i)" `run` State.put 1
          b_1 = whenBack (== 1) $ binds' $ do
            on (press $ ctrl $ WKS.xK_i) `as` "P(C-i)" `run` State.put 0
            on (press $ alt $ ctrl $ WKS.xK_x) `as` "P(M-C-x)" `run` return ()
          rev _ _ i = justBefore $ p ("Input: " ++ (unpack $ WBD.describe i))
          b = revise rev $ startFrom (0 :: Int) $ b_base <> b_0 <> b_1
          exp_descs = [ ["P(C-g)", "R(M-C-x)", "P(C-i)"],
                        ["P(C-g)", "R(M-C-x)", "P(C-i)", "P(M-C-x)"],
                        ["P(C-g)", "R(M-C-x)", "P(C-i)"]
                      ]
      p "Input C-i C-i M-C-x. If wildBind is still blocked, then type C-g."
      result <- waitCatch =<< async (wildBind' opt b f)
      got_descs <- fmap ((map . map) snd . reverse) $ readIORef got_binds_rev
      length got_descs `shouldBe` length exp_descs
      forM_ (zip got_descs exp_descs) $ uncurry shouldMatchList
      case result of
       Right _ -> expectationFailure "expects an exception, but nothing happened."
       Left e  -> fromException e `shouldBe` (Just $ MyException "OK")

  describe "X11Front - NumPadUnlocked" $ do
    it "should grab/ungrab keys" $ maybeRun $ withFrontEndForTest $ \f -> do
      let grabCase' :: FrontEnd ActiveWindow NumPad.NumPadUnlocked -> Expectation
          grabCase' = grabCase
      grabCase' f
  describe "X11Front - NumPadLocked" $ do
    it "should grab/ungrab keys" $ maybeRun $ withFrontEndForTest $ \f -> do
      let grabCase' :: FrontEnd ActiveWindow NumPad.NumPadLocked -> Expectation
          grabCase' = grabCase
      grabCase' f
  describe "X11Front - normal modified keys (pressed)" $ do
    it "should distinguish modifiers" $ maybeRun $ withFrontEndForTest $ \f -> do
      let inputs = [ ctrl Xlib.xK_i,
                     ctrl $ alt Xlib.xK_i,
                     super Xlib.xK_i,
                     shift $ super Xlib.xK_I
                   ]
      withGrabs f inputs $ do
        p ("Grabbed " ++ (intercalate ", " $ map describeStr inputs))
        forM_ inputs $ \input -> do
          p ("Push " ++ describeStr input)
          press_ev <- frontNextEvent f
          p ("Got event: " ++ show press_ev)
          press_ev `shouldBe` FEInput input
          release_ev <- frontNextEvent f
          p ("Got event: " ++ show release_ev)
          release_ev `shouldBe` (FEInput $ input { xKeyEventType = KeyRelease })
  describe "X11Front - Either" $ do
    it "should combine input types" $ maybeRun $ withFrontEndForTest $ \f -> do
      let inputs = [Left NumPad.NumLPeriod, Right NumPad.NumDelete]
      withGrabs f inputs $ do
        forM_ inputs $ \input -> do
          p ("Push " ++ show input)
          ev <- frontNextEvent f
          ev `shouldBe` FEInput input
