
module Data.ByteString.ShellEscape.Escape where

import Data.ByteString

{-| A type class for objects that represent an intermediate state of
    escaping.
 -}
class Escape t where
  -- |    Transform a 'ByteString' into the escaped intermediate form.
  escape                    ::  ByteString -> t
  -- |    Recover the original 'ByteString'.
  unescape                  ::  t -> ByteString
  -- |    Yield the escaped 'ByteString'.
  bytes                     ::  t -> ByteString

instance Escape ByteString where
  escape                     =  id
  unescape                   =  id
  bytes                      =  id

