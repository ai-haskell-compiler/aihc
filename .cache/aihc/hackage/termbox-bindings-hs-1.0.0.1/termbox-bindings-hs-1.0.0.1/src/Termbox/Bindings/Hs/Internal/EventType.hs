module Termbox.Bindings.Hs.Internal.EventType
  ( Tb_event_type
      ( Tb_event_type,
        TB_EVENT_KEY,
        TB_EVENT_MOUSE,
        TB_EVENT_RESIZE
      ),
  )
where

import Data.Word (Word8)
import Termbox.Bindings.C (_TB_EVENT_KEY, _TB_EVENT_MOUSE, _TB_EVENT_RESIZE)

-- | An event type.
newtype Tb_event_type
  = Tb_event_type Word8
  deriving stock (Eq)

instance Show Tb_event_type where
  show = \case
    TB_EVENT_KEY -> "TB_EVENT_KEY"
    TB_EVENT_MOUSE -> "TB_EVENT_MOUSE"
    TB_EVENT_RESIZE -> "TB_EVENT_RESIZE"

pattern TB_EVENT_KEY :: Tb_event_type
pattern TB_EVENT_KEY <-
  ((== Tb_event_type _TB_EVENT_KEY) -> True)
  where
    TB_EVENT_KEY = Tb_event_type _TB_EVENT_KEY

pattern TB_EVENT_MOUSE :: Tb_event_type
pattern TB_EVENT_MOUSE <-
  ((== Tb_event_type _TB_EVENT_MOUSE) -> True)
  where
    TB_EVENT_MOUSE = Tb_event_type _TB_EVENT_MOUSE

pattern TB_EVENT_RESIZE :: Tb_event_type
pattern TB_EVENT_RESIZE <-
  ((== Tb_event_type _TB_EVENT_RESIZE) -> True)
  where
    TB_EVENT_RESIZE = Tb_event_type _TB_EVENT_RESIZE

-- N.B. This requires Tb_event_type to remain abstract
{-# COMPLETE TB_EVENT_KEY, TB_EVENT_MOUSE, TB_EVENT_RESIZE #-}
