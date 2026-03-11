module Termbox.Internal.Event
  ( Event (..),
    poll,
  )
where

import Data.Int (Int32)
import GHC.Generics (Generic)
import qualified Termbox.Bindings.Hs
import Termbox.Internal.Key (Key (KeyChar), parseKey)
import Termbox.Internal.Mouse (Mouse (..), MouseButton (..))
import Termbox.Internal.Pos (Pos (..))
import Termbox.Internal.Size (Size (..))
import Prelude hiding (mod)

-- | An input event.
data Event e
  = -- | Key event
    EventKey !Key
  | -- | Resize event
    EventResize !Size
  | -- | Mouse event
    EventMouse !Mouse
  | -- | User event
    EventUser !e
  deriving stock (Eq, Generic, Show)

-- | Poll for an event.
poll :: IO (Event e)
poll =
  poll_ >>= \case
    Left () -> poll
    Right event -> pure event

poll_ :: IO (Either () (Event e))
poll_ = do
  result <- Termbox.Bindings.Hs.tb_poll_event
  pure (parseEvent <$> result)

-- Parse an Event from a TbEvent.
parseEvent :: Termbox.Bindings.Hs.Tb_event -> Event e
parseEvent
  Termbox.Bindings.Hs.Tb_event
    { Termbox.Bindings.Hs.type_,
      Termbox.Bindings.Hs.mod = _,
      Termbox.Bindings.Hs.key,
      Termbox.Bindings.Hs.ch,
      Termbox.Bindings.Hs.w,
      Termbox.Bindings.Hs.h,
      Termbox.Bindings.Hs.x,
      Termbox.Bindings.Hs.y
    } =
    case type_ of
      Termbox.Bindings.Hs.TB_EVENT_KEY -> EventKey (if ch == '\0' then parseKey key else KeyChar ch)
      Termbox.Bindings.Hs.TB_EVENT_RESIZE ->
        EventResize
          Size
            { width = fromIntegral @Int32 @Int w,
              height = fromIntegral @Int32 @Int h
            }
      Termbox.Bindings.Hs.TB_EVENT_MOUSE ->
        EventMouse
          Mouse
            { button = MouseButton key,
              pos =
                Pos
                  { row = fromIntegral @Int32 @Int y,
                    col = fromIntegral @Int32 @Int x
                  }
            }
