
module Data.Radius.StreamPut.Monadic (
  -- * DSL to build attribute list of packet
  AttributePutM, extractAttributes,

  tellA,

  -- * low-level definitions
  AtValueEncode,
  exAttribute, attribute,
  ) where

import Control.Applicative (pure)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Serialize.Put (Put, runPut)

import Data.Radius.Scalar (AtText, AtString, AtInteger, AtIpV4)
import Data.Radius.Attribute
  (Attribute (..), untypeNumber, TypedNumber, Attribute' (..))
import qualified Data.Radius.StreamPut.Base as Base


class AtValueEncode a where
  atValueEncode :: a -> Put

instance AtValueEncode AtText where
  atValueEncode = Base.atText

instance AtValueEncode AtString where
  atValueEncode = Base.atString

instance AtValueEncode AtInteger where
  atValueEncode = Base.atInteger

instance AtValueEncode AtIpV4 where
  atValueEncode = Base.atIpV4

-- | Context monad type to build attribute list of packet
type AttributePutM v = Writer (DList (Attribute' v))

exAttribute :: (a -> Put) -> Attribute v a -> AttributePutM v ()
exAttribute vp (Attribute n v) =
  tell . pure . Attribute' (untypeNumber n) . runPut $ vp v

attribute :: AtValueEncode a => Attribute v a -> AttributePutM v ()
attribute = exAttribute atValueEncode

-- | Add attribute key and value into monadic context
tellA :: AtValueEncode a => TypedNumber v a -> a -> AttributePutM v ()
tellA = (attribute .) . Attribute

-- | Extract attribute list from context
extractAttributes :: AttributePutM v a -> [Attribute' v]
extractAttributes w = DList.toList dl  where
  (_, dl) = runWriter w
