-- | References to values that a program can send to a different process.
--
-- The @static@ form of the StaticPointers extension makes each
-- 'StaticPtr'. GHC also records each one in a static pointer table. aihc
-- does not make this table yet. Thus 'unsafeLookupStaticPtr' finds no
-- static pointer, and 'staticPtrKeys' is empty.
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

import GHC.StaticPtr.Internal
import Prelude

-- | The value that a static pointer refers to.
deRefStaticPtr :: StaticPtr a -> a
deRefStaticPtr (StaticPtr _ _ value) = value

-- | The key of a static pointer.
staticKey :: StaticPtr a -> StaticKey
staticKey (StaticPtr key _ _) = key

-- | Find the static pointer with the given key.
--
-- aihc does not make a static pointer table yet, thus the result is always
-- 'Nothing'.
unsafeLookupStaticPtr :: StaticKey -> IO (Maybe (StaticPtr a))
unsafeLookupStaticPtr _ = return Nothing

-- | Where the @static@ form of a static pointer is in the program.
staticPtrInfo :: StaticPtr a -> StaticPtrInfo
staticPtrInfo (StaticPtr _ info _) = info

-- | The keys of all static pointers in the program.
--
-- aihc does not make a static pointer table yet, thus the result is always
-- empty.
staticPtrKeys :: IO [StaticKey]
staticPtrKeys = return []
