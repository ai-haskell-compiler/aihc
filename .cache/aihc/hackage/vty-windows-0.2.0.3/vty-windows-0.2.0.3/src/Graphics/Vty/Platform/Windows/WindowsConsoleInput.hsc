{-# LANGUAGE ForeignFunctionInterface, CPP #-}

-- | This module provides a function to obtain input events from the Win32 API
module Graphics.Vty.Platform.Windows.WindowsConsoleInput
    (KeyEventRecord(..),
     MouseEventRecord(..),
     WindowBufferSizeRecord(..),
     MenuEventRecord(..),
     FocusEventRecord(..),
     WinConsoleInputEvent(..),
     readConsoleInput) where

import Control.Monad (foldM)
import Foreign.C.Types ( CWchar(..) )
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import GHC.Ptr ( Ptr )
import System.Win32.Console
import System.Win32.Types
import System.IO (Handle)

#include <windows.h>

foreign import ccall unsafe "windows.h ReadConsoleInputW" cReadConsoleInput :: HANDLE -> Ptr WinConsoleInputEvent -> DWORD -> LPDWORD -> IO BOOL

-- | This type represents a keyboard input event. The structure is documented here:
-- https://learn.microsoft.com/en-us/windows/console/key-event-record-str
data KeyEventRecord = KeyEventRecordC
  { keyDown :: BOOL
  , repeatCount :: WORD
  , virtualKeyCode :: WORD
  , virtualScanCode :: WORD
  , uChar :: CWchar
  , controlKeyStateK :: DWORD
  } deriving (Eq, Show)

-- | This type represents a mouse event. The structure is documented here:
-- https://learn.microsoft.com/en-us/windows/console/mouse-event-record-str
data MouseEventRecord = MouseEventRecordC
  { mousePosition :: COORD
  , buttonState :: DWORD
  , controlKeyStateM :: DWORD
  , eventFlags :: DWORD
  } deriving (Eq, Show)

-- | This type represents a window size change event. The structure is documented here:
-- https://learn.microsoft.com/en-us/windows/console/window-buffer-size-record-str
newtype WindowBufferSizeRecord = WindowBufferSizeRecordC
  { windowSize :: COORD
  } deriving (Eq, Show)

-- | This type represents a window menu event. (Current ignored by VTY). The structure
-- is documented here: https://learn.microsoft.com/en-us/windows/console/menu-event-record-str
newtype MenuEventRecord = MenuEventRecordC
  { commandId :: UINT
  } deriving (Eq, Show)

-- | This type represents a window focus change event. The structure is documented here:
-- https://learn.microsoft.com/en-us/windows/console/focus-event-record-str
newtype FocusEventRecord = FocusEventRecordC
  { setFocus :: BOOL
  } deriving (Eq, Show)

-- | Description of a Windows console input event. Documented here:
-- https://learn.microsoft.com/en-us/windows/console/input-record-str
data WinConsoleInputEvent =
    KeyEventRecordU KeyEventRecord
  | MouseEventRecordU MouseEventRecord
  | WindowBufferSizeRecordU WindowBufferSizeRecord
  | MenuEventRecordU MenuEventRecord
  | FocusEventRecordU FocusEventRecord
  deriving (Eq, Show)

-- | A wrapper for the ReadConsoleInput Win32 API as documented here:
-- https://learn.microsoft.com/en-us/windows/console/readconsoleinput
readConsoleInput :: Ptr WinConsoleInputEvent -> Int -> Handle -> IO [WinConsoleInputEvent]
readConsoleInput inputRecordPtr maxEvents handle = withHandleToHANDLE handle (readConsoleInput' inputRecordPtr maxEvents)

readConsoleInput' :: Ptr WinConsoleInputEvent -> Int -> HANDLE -> IO [WinConsoleInputEvent]
readConsoleInput' inputRecordPtr maxEvents handle' =
    alloca $ \numEventsReadPtr -> do
        poke numEventsReadPtr 1
        _ <- cReadConsoleInput handle' inputRecordPtr (fromIntegral maxEvents) numEventsReadPtr
        numEvents <- peek numEventsReadPtr
        foldM addNextRecord [] [(fromIntegral numEvents - 1), (fromIntegral numEvents - 2)..0]
    where
        addNextRecord inputRecords idx = do
            inputRecord <- peekElemOff inputRecordPtr idx
            return (inputRecord:inputRecords)

instance Storable KeyEventRecord where
    sizeOf = const #{size KEY_EVENT_RECORD}
    alignment _ = #alignment KEY_EVENT_RECORD
    poke buf input = do
        (#poke KEY_EVENT_RECORD, bKeyDown)          buf (keyDown input)
        (#poke KEY_EVENT_RECORD, wRepeatCount)      buf (repeatCount input)
        (#poke KEY_EVENT_RECORD, wVirtualKeyCode)   buf (virtualKeyCode input)
        (#poke KEY_EVENT_RECORD, wVirtualScanCode)  buf (virtualScanCode input)
        (#poke KEY_EVENT_RECORD, uChar)             buf (uChar input)
        (#poke KEY_EVENT_RECORD, dwControlKeyState) buf (controlKeyStateK input)
    peek buf = do
        keyDown'          <- (#peek KEY_EVENT_RECORD, bKeyDown) buf
        repeatCount'      <- (#peek KEY_EVENT_RECORD, wRepeatCount) buf
        virtualKeyCode'   <- (#peek KEY_EVENT_RECORD, wVirtualKeyCode) buf
        virtualScanCode'  <- (#peek KEY_EVENT_RECORD, wVirtualScanCode) buf
        uChar'            <- (#peek KEY_EVENT_RECORD, uChar) buf
        controlKeyStateK' <- (#peek KEY_EVENT_RECORD, dwControlKeyState) buf
        return $ KeyEventRecordC keyDown' repeatCount' virtualKeyCode' virtualScanCode' uChar' controlKeyStateK'

instance Storable MouseEventRecord where
    sizeOf = const #{size MOUSE_EVENT_RECORD}
    alignment _ = #alignment MOUSE_EVENT_RECORD
    poke buf input = do
        (#poke MOUSE_EVENT_RECORD, dwMousePosition)   buf (mousePosition input)
        (#poke MOUSE_EVENT_RECORD, dwButtonState)     buf (buttonState input)
        (#poke MOUSE_EVENT_RECORD, dwControlKeyState) buf (controlKeyStateM input)
        (#poke MOUSE_EVENT_RECORD, dwEventFlags)      buf (eventFlags input)
    peek buf = do
        mousePosition'    <- (#peek MOUSE_EVENT_RECORD, dwMousePosition) buf
        buttonState'      <- (#peek MOUSE_EVENT_RECORD, dwButtonState) buf
        controlKeyStateM' <- (#peek MOUSE_EVENT_RECORD, dwControlKeyState) buf
        eventFlags'       <- (#peek MOUSE_EVENT_RECORD, dwEventFlags) buf
        return $ MouseEventRecordC mousePosition' buttonState' controlKeyStateM' eventFlags'

instance Storable WindowBufferSizeRecord where
    sizeOf = const #{size WINDOW_BUFFER_SIZE_RECORD}
    alignment _ = #alignment WINDOW_BUFFER_SIZE_RECORD
    poke buf input = do
        (#poke WINDOW_BUFFER_SIZE_RECORD, dwSize) buf (windowSize input)
    peek buf = do
        size' <- (#peek WINDOW_BUFFER_SIZE_RECORD, dwSize) buf
        return $ WindowBufferSizeRecordC size'

instance Storable MenuEventRecord where
    sizeOf = const #{size MENU_EVENT_RECORD}
    alignment _ = #alignment MENU_EVENT_RECORD
    poke buf input = do
        (#poke MENU_EVENT_RECORD, dwCommandId) buf (commandId input)
    peek buf = do
        commandId' <- (#peek MENU_EVENT_RECORD, dwCommandId) buf
        return $ MenuEventRecordC commandId'

instance Storable FocusEventRecord where
    sizeOf = const #{size FOCUS_EVENT_RECORD}
    alignment _ = #alignment FOCUS_EVENT_RECORD
    poke buf input = do
        (#poke FOCUS_EVENT_RECORD, bSetFocus) buf (setFocus input)
    peek buf = do
        setFocus' <- (#peek FOCUS_EVENT_RECORD, bSetFocus) buf
        return $ FocusEventRecordC setFocus'

instance Storable WinConsoleInputEvent where
    sizeOf = const #{size INPUT_RECORD}
    alignment _ = #alignment INPUT_RECORD

    poke buf (KeyEventRecordU key) = do
        (#poke INPUT_RECORD, EventType) buf (#{const KEY_EVENT} :: WORD)
        (#poke INPUT_RECORD, Event) buf key
    poke buf (MouseEventRecordU mouse) = do
        (#poke INPUT_RECORD, EventType) buf (#{const MOUSE_EVENT} :: WORD)
        (#poke INPUT_RECORD, Event) buf mouse
    poke buf (WindowBufferSizeRecordU window) = do
        (#poke INPUT_RECORD, EventType) buf (#{const WINDOW_BUFFER_SIZE_EVENT} :: WORD)
        (#poke INPUT_RECORD, Event) buf window
    poke buf (MenuEventRecordU menu) = do
        (#poke INPUT_RECORD, EventType) buf (#{const MENU_EVENT} :: WORD)
        (#poke INPUT_RECORD, Event) buf menu
    poke buf (FocusEventRecordU focus) = do
        (#poke INPUT_RECORD, EventType) buf (#{const FOCUS_EVENT} :: WORD)
        (#poke INPUT_RECORD, Event) buf focus

    peek buf = do
        event <- (#peek INPUT_RECORD, EventType) buf :: IO WORD
        case event of
          #{const KEY_EVENT} ->
              KeyEventRecordU `fmap` (#peek INPUT_RECORD, Event) buf
          #{const MOUSE_EVENT} ->
              MouseEventRecordU `fmap` (#peek INPUT_RECORD, Event) buf
          #{const WINDOW_BUFFER_SIZE_EVENT} ->
              WindowBufferSizeRecordU `fmap` (#peek INPUT_RECORD, Event) buf
          #{const MENU_EVENT} ->
              MenuEventRecordU `fmap` (#peek INPUT_RECORD, Event) buf
          #{const FOCUS_EVENT} ->
              FocusEventRecordU `fmap` (#peek INPUT_RECORD, Event) buf
          _ -> error $ "Unknown input event type " ++ show event
