module Termbox.Bindings.Hs.Internal.Event
  ( Tb_event (..),
    ceventToEvent,
  )
where

import Data.Int (Int32)
import GHC.Generics (Generic)
import qualified Termbox.Bindings.C as Termbox
import Termbox.Bindings.Hs.Internal.EventMod (Tb_event_mod (..))
import Termbox.Bindings.Hs.Internal.EventType (Tb_event_type (..))
import Termbox.Bindings.Hs.Internal.Key (Tb_key (..))
import Termbox.Bindings.Hs.Internal.Prelude (word32ToChar)
import Prelude hiding (mod)

-- | An event.
data Tb_event = Tb_event
  { type_ :: {-# UNPACK #-} !Tb_event_type,
    mod :: !(Maybe Tb_event_mod),
    key :: {-# UNPACK #-} !Tb_key,
    ch :: {-# UNPACK #-} !Char,
    w :: {-# UNPACK #-} !Int32,
    h :: {-# UNPACK #-} !Int32,
    x :: {-# UNPACK #-} !Int32,
    y :: {-# UNPACK #-} !Int32
  }
  deriving stock (Eq, Generic, Show)

ceventToEvent :: Termbox.Tb_event -> Tb_event
ceventToEvent Termbox.Tb_event {type_, mod, key, ch, w, h, x, y} =
  Tb_event
    { type_ = Tb_event_type type_,
      mod = if mod == 0 then Nothing else Just (Tb_event_mod mod),
      key = Tb_key key,
      ch = word32ToChar ch,
      w,
      h,
      x,
      y
    }
{-# INLINE ceventToEvent #-}
