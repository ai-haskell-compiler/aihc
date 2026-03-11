-- |
-- Module: WildBind.X11.Internal.FrontEnd
-- Description: WildBind FrontEnd implementation for X11
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __This is an internal module. Package users should not rely on this.__
module WildBind.X11.Internal.FrontEnd
    ( -- * X11Front
      X11Front (..)
    , withFrontEnd
    , withX11Front
    , makeFrontEnd
    , defaultRootWindow
    ) where


import           Control.Applicative                         (empty, (<$>))
import           Control.Concurrent                          (rtsSupportsBoundThreads)
import           Control.Concurrent.STM                      (TChan, atomically, newTChanIO,
                                                              tryReadTChan, writeTChan)
import           Control.Exception                           (bracket, throwIO)
import           Control.Monad                               (filterM, mapM_, when)
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.Trans.Cont                    (ContT (ContT), runContT)
import           Control.Monad.Trans.Maybe                   (MaybeT, runMaybeT)
import           Control.Monad.Trans.Writer                  (WriterT, execWriterT, tell)
import           Data.Bits                                   ((.|.))
import           Data.IORef                                  (IORef, newIORef, readIORef,
                                                              writeIORef)
import qualified Graphics.X11.Xlib                           as Xlib

import           WildBind                                    (FrontEnd (FrontEnd, frontDefaultDescription, frontNextEvent, frontSetGrab, frontUnsetGrab),
                                                              FrontEvent (FEChange, FEInput))
import qualified WildBind.Description                        as WBD

import qualified WildBind.X11.Internal.GrabMan               as GM
import           WildBind.X11.Internal.Key                   (KeyEventType (..), KeyMaskMap,
                                                              XKeyInput (..), getKeyMaskMap,
                                                              xGrabKey, xKeyEventToXKeyInput,
                                                              xUngrabKey)
import qualified WildBind.X11.Internal.NotificationDebouncer as Ndeb
import           WildBind.X11.Internal.Window                (ActiveWindow, Window,
                                                              defaultRootWindowForDisplay,
                                                              emptyWindow, getActiveWindow,
                                                              winClass, winInstance, winName)

-- | The X11 front-end. @k@ is the input key type.
--
-- This is the implementation of the 'FrontEnd' given by
-- 'withFrontEnd' function. With this object, you can do more advanced
-- actions. See "WildBind.X11.Emulate".
--
-- 'X11Front' is relatively low-level interface, so it's more likely
-- for this API to change in the future than 'FrontEnd'.
--
-- @since 0.2.0.0
data X11Front k
  = X11Front
      { x11Display          :: Xlib.Display
      , x11Debouncer        :: Ndeb.Debouncer
      , x11PrevActiveWindow :: IORef (Maybe ActiveWindow)
      , x11PendingEvents    :: TChan (FrontEvent ActiveWindow k)
      , x11KeyMaskMap       :: KeyMaskMap
      , x11GrabMan          :: IORef (GM.GrabMan k)
      }

x11PopPendingEvent :: X11Front k -> IO (Maybe (FrontEvent ActiveWindow k))
x11PopPendingEvent f = atomically $ tryReadTChan $ x11PendingEvents f

x11UnshiftPendingEvents :: X11Front k -> [FrontEvent ActiveWindow k] -> IO ()
x11UnshiftPendingEvents f = atomically . mapM_ (writeTChan $ x11PendingEvents f)

openMyDisplay :: IO Xlib.Display
openMyDisplay = Xlib.openDisplay ""

-- | Initialize and obtain 'FrontEnd' for X11, and run the given
-- action.
--
-- The X11 'FrontEnd' watches and provides 'ActiveWindow' as the
-- front-end state. 'ActiveWindow' keeps information about the window
-- currently active. As for the input type @i@, this 'FrontEnd' gets
-- keyboard events from the X server.
--
-- CAVEATS
--
-- Code using this function must be compiled
-- __with @-threaded@ option enabled__ in @ghc@. Otherwise, it aborts.
--
-- Basically you should call this function directly under @main@. This
-- is because this function calls some low-level X11 functions to
-- initialize the X11 client, which should be done first.
--
-- Because this 'FrontEnd' currently uses @XGrabKey(3)@ to get the
-- input, it may cause some weird behavior such as:
--
-- - Every input event makes the active window lose focus
--   temporarily. This may result in flickering cursor, for example. See
--   also: https://stackoverflow.com/questions/15270420/
--
-- - Key input is captured only while the first grabbed key is
--   pressed. For example, if @(release xK_a)@ and @(release xK_b)@
--   are bound, and you input @(press xK_a)@, @(press xK_b)@, @(release xK_a)@,
--   @(release xK_b)@, the last @(release xK_b)@ is NOT captured
--   because key grab ends with @(release xK_a)@.
withFrontEnd :: (XKeyInput i, WBD.Describable i, Ord i) => (FrontEnd ActiveWindow i -> IO a) -> IO a
withFrontEnd action = withX11Front' "WildBind.X11.withFrontEnd" $ \x11front -> action (makeFrontEnd x11front)

-- | Same as 'withFrontEnd', but it creates 'X11Front'. To create
-- 'FrontEnd', use 'makeFrontEnd'.
--
-- @since 0.2.0.0
withX11Front :: (X11Front k -> IO a) -> IO a
withX11Front = withX11Front' "WildBind.X11.withX11Front"

withX11Front' :: String -- ^ function name used in the error message.
              -> (X11Front k -> IO a)
              -> IO a
withX11Front' func_name = if rtsSupportsBoundThreads then impl else error_impl where
  impl = runContT $ do
    liftIO $ doInitThreads
    disp <- ContT $ bracket openMyDisplay Xlib.closeDisplay
    keymask_map <- liftIO $ getKeyMaskMap disp
    notif_disp <- ContT $ bracket openMyDisplay Xlib.closeDisplay
    debouncer <- ContT $ Ndeb.withDebouncer notif_disp
    liftIO $ Xlib.selectInput disp (Xlib.defaultRootWindow disp)
      (Xlib.substructureNotifyMask .|. Ndeb.xEventMask)
    awin_ref <- liftIO $ newIORef Nothing
    pending_events <- liftIO $ newTChanIO
    grab_man <- liftIO $ GM.new keymask_map disp (Xlib.defaultRootWindow disp)
    liftIO $ Ndeb.notify debouncer
    return $ X11Front disp debouncer awin_ref pending_events keymask_map grab_man
  error_impl _ = throwIO $ userError ("You need to build with -threaded option when you use " ++ func_name ++ " function.")
  doInitThreads = do
    ret <- Xlib.initThreads
    when (ret == 0) $ do
      throwIO $ userError ("Failure in XInitThreads.")


tellElem :: Monad m => a -> WriterT [a] m ()
tellElem a = tell [a]

data InternalEvent
  = IEKey KeyEventType
  | IEDebounced
  | IEActiveWindow
  | IEUnknown

identifyEvent :: Ndeb.Debouncer -> Xlib.XEventPtr -> IO InternalEvent
identifyEvent deb xev = do
  xtype <- Xlib.get_EventType xev
  identify xtype
  where
    identify xtype | xtype == Xlib.keyPress = return $ IEKey KeyPress
                   | xtype == Xlib.keyRelease = return $ IEKey KeyRelease
                   | xtype == Xlib.configureNotify || xtype == Xlib.destroyNotify = return $ IEActiveWindow
                   | otherwise = do
                       is_deb_event <- Ndeb.isDebouncedEvent deb xev
                       if is_deb_event
                         then return IEDebounced
                         else return IEUnknown

convertEvent :: (XKeyInput k) => KeyMaskMap -> Xlib.Display -> Ndeb.Debouncer -> Xlib.XEventPtr -> IO [FrontEvent ActiveWindow k]
convertEvent kmmap disp deb xev = execWriterT $ convertEventWriter where
  tellChangeEvent = (tellElem . FEChange) =<< (liftIO $ getActiveWindow disp)
  convertEventWriter :: XKeyInput k => WriterT [FrontEvent ActiveWindow k] IO ()
  convertEventWriter = do
    in_event <- liftIO $ identifyEvent deb xev
    case in_event of
     IEKey ev_type -> do
       let key_ev = Xlib.asKeyEvent xev
       tellChangeEvent
       (maybe (return ()) tellElem) =<< (liftIO $ runMaybeT (FEInput <$> xKeyEventToXKeyInput kmmap ev_type key_ev))
     IEDebounced -> tellChangeEvent
     IEActiveWindow -> liftIO (Ndeb.notify deb) >> return ()
     IEUnknown -> return ()

isSignificantEvent :: X11Front k -> FrontEvent ActiveWindow k -> IO Bool
isSignificantEvent front (FEChange new_state) = do
  m_old_state <- liftIO $ readIORef $ x11PrevActiveWindow front
  case m_old_state of
   Nothing        -> return True
   Just old_state -> return (not $ new_state == old_state)
isSignificantEvent _ _ = return True

updateState :: X11Front k -> FrontEvent ActiveWindow k -> IO ()
updateState front fev = case fev of
  (FEInput _)  -> return ()
  (FEChange s) -> writeIORef (x11PrevActiveWindow front) (Just s)

nextEvent :: (XKeyInput k) => X11Front k -> IO (FrontEvent ActiveWindow k)
nextEvent handle = loop where
  loop = do
    mpending <- x11PopPendingEvent handle
    case mpending of
      Just eve -> return eve
      Nothing  -> nextEventFromX11
  nextEventFromX11 = Xlib.allocaXEvent $ \xev -> do
    Xlib.nextEvent (x11Display handle) xev
    got_events <- processEvents xev
    case got_events of
      [] -> loop
      (eve : rest) -> do
        x11UnshiftPendingEvents handle rest
        return eve
  processEvents xev = do
    fevents <- filterM (isSignificantEvent handle)
               =<< convertEvent (x11KeyMaskMap handle) (x11Display handle) (x11Debouncer handle) xev
    mapM_ (updateState handle) fevents
    return fevents

-- | Create 'FrontEnd' from 'X11Front' object.
--
-- @since 0.2.0.0
makeFrontEnd :: (XKeyInput k, WBD.Describable k, Ord k) => X11Front k -> FrontEnd ActiveWindow k
makeFrontEnd f = FrontEnd { frontDefaultDescription = WBD.describe,
                            frontSetGrab = runGrab GM.DoSetGrab,
                            frontUnsetGrab = runGrab GM.DoUnsetGrab,
                            frontNextEvent = nextEvent f
                          }
  where
    runGrab = GM.modify (x11GrabMan f)

-- | Get the default root window.
--
-- @since 0.2.0.0
defaultRootWindow :: X11Front k -> Window
defaultRootWindow = defaultRootWindowForDisplay . x11Display
