-- | References to values that a program can send to a different process.
--
-- In GHC, the @static@ form of the StaticPointers extension makes each
-- 'StaticPtr', and the compiler records each one in a static pointer table.
-- aihc does not support the @static@ form yet. Thus a program cannot make a
-- 'StaticPtr', and the static pointer table is empty. The types and the
-- operations are here, so that code that refers to them compiles.
module GHC.StaticPtr
  ( StaticPtr,
    deRefStaticPtr,
    StaticKey,
    staticKey,
    unsafeLookupStaticPtr,
    StaticPtrInfo (..),
    staticPtrInfo,
    staticPtrKeys,
    IsStatic (..),
  )
where

import Data.Typeable (Typeable)
import GHC.Fingerprint (Fingerprint)
import Prelude

-- | A reference to a value that has the same key in each process that runs
-- the same program.
data StaticPtr a = StaticPtr StaticKey StaticPtrInfo a

-- | The value that a static pointer refers to.
deRefStaticPtr :: StaticPtr a -> a
deRefStaticPtr (StaticPtr _ _ value) = value

-- | The key of a static pointer. The key is the same in each process that
-- runs the same program.
type StaticKey = Fingerprint

-- | The key of a static pointer.
staticKey :: StaticPtr a -> StaticKey
staticKey (StaticPtr key _ _) = key

-- | Find the static pointer with the given key.
--
-- The static pointer table is empty, thus the result is always 'Nothing'.
unsafeLookupStaticPtr :: StaticKey -> IO (Maybe (StaticPtr a))
unsafeLookupStaticPtr _ = return Nothing

-- | Where the @static@ form of a static pointer is in the program.
data StaticPtrInfo = StaticPtrInfo
  { -- | The unit of the module.
    spInfoUnitId :: String,
    -- | The name of the module.
    spInfoModuleName :: String,
    -- | The line and the column of the @static@ form.
    spInfoSrcLoc :: (Int, Int)
  }
  deriving (Show)

-- | Where the @static@ form of a static pointer is in the program.
staticPtrInfo :: StaticPtr a -> StaticPtrInfo
staticPtrInfo (StaticPtr _ info _) = info

-- | The keys of all static pointers in the program.
--
-- The static pointer table is empty, thus the result is always empty.
staticPtrKeys :: IO [StaticKey]
staticPtrKeys = return []

-- | The types that a static pointer can become. In GHC, the @static@ form
-- has the type @p a@ for each instance @p@ of this class.
class IsStatic p where
  fromStaticPtr :: (Typeable a) => StaticPtr a -> p a

instance IsStatic StaticPtr where
  fromStaticPtr = id
