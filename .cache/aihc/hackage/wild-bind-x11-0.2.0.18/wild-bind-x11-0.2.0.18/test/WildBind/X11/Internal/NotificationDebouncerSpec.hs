module WildBind.X11.Internal.NotificationDebouncerSpec
    ( spec
    ) where

import           Test.Hspec

import qualified Graphics.X11.Xlib                           as Xlib
import qualified Graphics.X11.Xlib.Extras                    as XlibE
import qualified WildBind.X11.Internal.NotificationDebouncer as Deb

import           WildBind.X11.TestUtil                       (checkIfX11Available)

spec :: Spec
spec = checkIfX11Available $ do
  describe "notify" $ do
    it "should create an X event that passes isDebouncedEvent" $ do
      disp <- Xlib.openDisplay ""
      Xlib.selectInput disp (Xlib.defaultRootWindow disp) Deb.xEventMask
      deb_disp <- Xlib.openDisplay ""
      Deb.withDebouncer deb_disp $ \deb -> do
        Deb.notify deb
        (Xlib.allocaXEvent $ waitForDebouncedEvent deb disp) `shouldReturn` True

-- showDisplay :: Xlib.Display -> String
-- showDisplay disp = show disp ++ "(conn number = "++ show (Xlib.connectionNumber disp) ++")"

waitForDebouncedEvent :: Deb.Debouncer -> Xlib.Display -> Xlib.XEventPtr -> IO Bool
waitForDebouncedEvent deb disp xev = doit 0 where
  doit :: Int -> IO Bool
  doit count = do
    Xlib.nextEvent disp xev
    ret <- Deb.isDebouncedEvent deb xev
    showXEvent disp xev >>= \xev_str -> putStrLn ("Got event: " ++ xev_str)
    if ret || (count > 20)
      then return ret
      else doit (count + 1)


showAtom :: Xlib.Display -> Xlib.Atom -> IO String
showAtom disp atom = do
  name <- Xlib.getAtomName disp atom
  return (show atom ++ " ("++ show name ++")")

showXEvent :: Xlib.Display -> Xlib.XEventPtr -> IO String
showXEvent disp xev = do
  ev <- XlibE.getEvent xev
  case ev of
    XlibE.ClientMessageEvent _ _ _ _ _ got_type _ -> do
      got_name <- showAtom disp got_type
      return (show ev ++ ", message type = " ++ got_name)
    _ -> return (show ev)

