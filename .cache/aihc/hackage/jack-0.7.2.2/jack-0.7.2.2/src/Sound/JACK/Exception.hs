module Sound.JACK.Exception (
    ToString(toString), toStringWithHead,
    All,
    ThrowsStatus       (status),       Status(..),
    ThrowsPortRegister (portRegister), PortRegister(..),
    ThrowsPortMismatch (portMismatch), PortMismatch(..), PortMismatchKind(..),
    ThrowsErrno        (errno),        Errno(..),
    ) where

import qualified Sound.JACK.FFI as JackFFI

import qualified Foreign.C.Error as FE
import qualified Data.EnumBitSet as ES
import Data.Bits (Bits, )



type All = Status (PortRegister (PortMismatch FE.Errno))

toStringWithHead ::
    All -> String
toStringWithHead e =
    "JACK exception: " ++ toString e

class ToString e where
    toString :: e -> String

instance (Bits w, Enum a, Show a) => ToString (ES.T w a) where
    toString = show . ES.toEnums


class ThrowsStatus e where
    status :: JackFFI.StatusSet -> e

data Status e =
     Status JackFFI.StatusSet
   | NoStatus e

instance ToString e => ToString (Status e) where
    toString e0 =
        case e0 of
            Status st -> toString st
            NoStatus e1 -> toString e1


instance ThrowsStatus (Status e) where
    status = Status

instance ThrowsPortRegister e => ThrowsPortRegister (Status e) where
    portRegister = NoStatus portRegister

instance ThrowsPortMismatch e => ThrowsPortMismatch (Status e) where
    portMismatch = NoStatus . portMismatch

instance ThrowsErrno e => ThrowsErrno (Status e) where
    errno = NoStatus . errno

{-
instance ThrowsStatus (ES.T w a) where
    status = id
-}


class ThrowsPortRegister e where
    portRegister :: e

data PortRegister e =
     PortRegister
   | NoPortRegister e

instance ToString e => ToString (PortRegister e) where
    toString e0 =
        case e0 of
          PortRegister -> "could not register port"
          NoPortRegister e1 -> toString e1

instance ThrowsStatus e => ThrowsStatus (PortRegister e) where
    status = NoPortRegister . status

instance ThrowsPortRegister (PortRegister e) where
    portRegister = PortRegister

instance ThrowsPortMismatch e => ThrowsPortMismatch (PortRegister e) where
    portMismatch = NoPortRegister . portMismatch

instance ThrowsErrno e => ThrowsErrno (PortRegister e) where
    errno = NoPortRegister . errno



class ThrowsPortMismatch e where
    portMismatch :: PortMismatchKind -> e

data PortMismatch e =
     PortMismatch PortMismatchKind
   | NoPortMismatch e

data PortMismatchKind = TypeMismatch | DirectionMismatch

instance ToString e => ToString (PortMismatch e) where
    toString e0 =
        case e0 of
          PortMismatch TypeMismatch -> "port not of excpected type"
          PortMismatch DirectionMismatch -> "port does not support excpected direction"
          NoPortMismatch e1 -> toString e1

instance ThrowsStatus e => ThrowsStatus (PortMismatch e) where
    status = NoPortMismatch . status

instance ThrowsPortRegister e => ThrowsPortRegister (PortMismatch e) where
    portRegister = NoPortMismatch portRegister

instance ThrowsPortMismatch (PortMismatch e) where
    portMismatch = PortMismatch

instance ThrowsErrno e => ThrowsErrno (PortMismatch e) where
    errno = NoPortMismatch . errno



class ThrowsErrno e where
    errno :: FE.Errno -> e

data Errno e =
     Errno FE.Errno
   | NoErrno e

instance ToString e => ToString (Errno e) where
    toString e0 =
        case e0 of
          Errno en -> toString en
          NoErrno e1 -> toString e1

instance ToString FE.Errno where
    toString (FE.Errno en) = "error code " ++ show en

instance ThrowsStatus e => ThrowsStatus (Errno e) where
    status = NoErrno . status

instance ThrowsPortRegister e => ThrowsPortRegister (Errno e) where
    portRegister = NoErrno portRegister

instance ThrowsPortMismatch e => ThrowsPortMismatch (Errno e) where
    portMismatch = NoErrno . portMismatch

instance ThrowsErrno (Errno e) where
    errno = Errno

instance ThrowsErrno FE.Errno where
    errno = id
