{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}

-- | This module provides 'Proxy' values for various types from the @base@
--   library and functions to use these values as hints for type inference.
module Type.Hint
  (
  -- * Hinting functions
    hintType
  , hintType1
  , hintTypeArg
  , hintType2
  , hintType2Arg1
  , hintType2Arg2
  , hintType3
  , hintType3Arg1
  , hintType3Arg2
  , hintType3Arg3
  -- * Standard types proxies
  , Proxy(..)
  , aUnit
  , aChar
  , anInteger
  , anInt
  , anInt8
  , anInt16
  , anInt32
  , anInt64
  , aWord
  , aWord8
  , aWord16
  , aWord32
  , aWord64
  , aRatio
  , aRatioOf
  , aRational
  , aFixed
  , aFixedOf
  , aUni
  , aDeci
  , aCenti
  , aMilli
  , aMicro
  , aNano
  , aPico
  , aFloat
  , aDouble
  , aMaybe
  , aMaybeOf
  , aPair
  , aPairOf
  , aTriple
  , aTripleOf
  , anEither
  , anEitherOf
  , aList
  , aListOf
  , anIo
  , anIoOf
  , anIoRef
  , anIoRefOf
  , anSt
  , anStOf
  , anStRef
  , anStRefOf
  ) where

import Data.Proxy (Proxy(..))
import Data.Word
import Data.Int
import Data.Fixed
import Data.Ratio
import Data.IORef (IORef)
import Data.STRef (STRef)
import Control.Monad.ST (ST)

infixl 1 `hintType`,
         `hintType1`, `hintTypeArg`,
         `hintType2`, `hintType2Arg1`, `hintType2Arg2`,
         `hintType3`, `hintType3Arg1`, `hintType3Arg2`, `hintType3Arg3`

-- | Hint the type system about the type.
hintType ∷ α → p α → α
hintType = const
{-# INLINE hintType #-}

-- | Hint the type system about the type constructor.
hintType1 ∷ f α → p f → f α
hintType1 = const
{-# INLINE hintType1 #-}

-- | Hint the type system about the type argument.
hintTypeArg ∷ f α → p α → f α
hintTypeArg = const
{-# INLINE hintTypeArg #-}

-- | Hint the type system about the two-argument type constructor.
hintType2 ∷ f α β → p f → f α β
hintType2 = const
{-# INLINE hintType2 #-}

-- | Hint the type system about the first type argument.
hintType2Arg1 ∷ f α β → p α → f α β
hintType2Arg1 = const
{-# INLINE hintType2Arg1 #-}

-- | Hint the type system about the second type argument.
hintType2Arg2 ∷ f α β → p β → f α β
hintType2Arg2 = const
{-# INLINE hintType2Arg2 #-}

-- | Hint the type system about the three-argument type constructor.
hintType3 ∷ f α β γ → p f → f α β γ
hintType3 = const
{-# INLINE hintType3 #-}

-- | Hint the type system about the first type argument.
hintType3Arg1 ∷ f α β γ → p α → f α β γ
hintType3Arg1 = const
{-# INLINE hintType3Arg1 #-}

-- | Hint the type system about the second type argument.
hintType3Arg2 ∷ f α β γ → p β → f α β γ
hintType3Arg2 = const
{-# INLINE hintType3Arg2 #-}

-- | Hint the type system about the third type argument.
hintType3Arg3 ∷ f α β γ → p γ → f α β γ
hintType3Arg3 = const
{-# INLINE hintType3Arg3 #-}

-- | /()/ proxy value.
aUnit ∷ Proxy ()
aUnit = Proxy

-- | 'Char' proxy value.
aChar ∷ Proxy Char
aChar = Proxy

-- | 'Integer' proxy value.
anInteger ∷ Proxy Integer
anInteger = Proxy

-- | 'Int' proxy value.
anInt ∷ Proxy Int
anInt = Proxy

-- | 'Int8' proxy value.
anInt8 ∷ Proxy Int8
anInt8 = Proxy

-- | 'Int16' proxy value.
anInt16 ∷ Proxy Int16
anInt16 = Proxy

-- | 'Int32' proxy value.
anInt32 ∷ Proxy Int32
anInt32 = Proxy

-- | 'Int64' proxy value.
anInt64 ∷ Proxy Int64
anInt64 = Proxy

-- | 'Word' proxy value.
aWord ∷ Proxy Word
aWord = Proxy

-- | 'Word8' proxy value.
aWord8 ∷ Proxy Word8
aWord8 = Proxy

-- | 'Word16' proxy value.
aWord16 ∷ Proxy Word16
aWord16 = Proxy

-- | 'Word32' proxy value.
aWord32 ∷ Proxy Word32
aWord32 = Proxy

-- | 'Word64' proxy value.
aWord64 ∷ Proxy Word64
aWord64 = Proxy

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
-- | 'Ratio' proxy value.
aRatio ∷ Proxy Ratio
aRatio = Proxy
#endif

-- | 'Ratio' /α/ proxy value.
aRatioOf ∷ Proxy α → Proxy (Ratio α)
aRatioOf _ = Proxy

-- | 'Rational' proxy value.
aRational ∷ Proxy Rational
aRational = Proxy

-- | 'Fixed' proxy value.
aFixed ∷ Proxy Fixed
aFixed = Proxy

-- | 'Fixed' /α/ proxy value.
aFixedOf ∷ Proxy α → Proxy (Fixed α)
aFixedOf _ = Proxy

-- | 'Uni' proxy value.
aUni ∷ Proxy Uni
aUni = Proxy

-- | 'Deci' proxy value.
aDeci ∷ Proxy Deci
aDeci = Proxy

-- | 'Centi' proxy value.
aCenti ∷ Proxy Centi
aCenti = Proxy

-- | 'Milli' proxy value.
aMilli ∷ Proxy Milli
aMilli = Proxy

-- | 'Micro' proxy value.
aMicro ∷ Proxy Micro
aMicro = Proxy

-- | 'Nano' proxy value.
aNano ∷ Proxy Nano
aNano = Proxy

-- | 'Pico' proxy value.
aPico ∷ Proxy Pico
aPico = Proxy

-- | 'Float' proxy value.
aFloat ∷ Proxy Float
aFloat = Proxy

-- | 'Double' proxy value.
aDouble ∷ Proxy Double
aDouble = Proxy

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
-- | 'Maybe' proxy value.
aMaybe ∷ Proxy Maybe
aMaybe = Proxy
#endif

-- | 'Maybe' /α/ proxy value.
aMaybeOf ∷ Proxy α → Proxy (Maybe α)
aMaybeOf _ = Proxy

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
-- | Pair proxy value.
aPair ∷ Proxy (,)
aPair = Proxy
#endif

-- | @(/α/, /β/)@ proxy value.
aPairOf ∷ Proxy α → Proxy β → Proxy (α, β)
aPairOf _ _ = Proxy

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
-- | Triple proxy value.
aTriple ∷ Proxy (,,)
aTriple = Proxy
#endif

-- | @(/α/, /β/, /γ/)@ proxy value.
aTripleOf ∷ Proxy α → Proxy β → Proxy γ → Proxy (α, β, γ)
aTripleOf _ _ _ = Proxy

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
-- | 'Either' proxy value.
anEither ∷ Proxy Either
anEither = Proxy
#endif

-- | 'Either' /α/ /β/ proxy value.
anEitherOf ∷ Proxy α → Proxy β → Proxy (Either α β)
anEitherOf _ _ = Proxy

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
-- | List proxy value.
aList ∷ Proxy []
aList = Proxy
#endif

-- | List of /α/ proxy value.
aListOf ∷ Proxy α → Proxy ([α])
aListOf _ = Proxy

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
-- | 'IO' proxy value.
anIo ∷ Proxy IO
anIo = Proxy
#endif

-- | 'IO' /α/ proxy value.
anIoOf ∷ Proxy α → Proxy (IO α)
anIoOf _ = Proxy

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
-- | 'IORef' proxy value.
anIoRef ∷ Proxy IORef
anIoRef = Proxy
#endif

-- | 'IORef' /α/ proxy value.
anIoRefOf ∷ Proxy α → Proxy (IORef α)
anIoRefOf _ = Proxy

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
-- | 'ST' proxy value.
anSt ∷ Proxy ST
anSt = Proxy
#endif

-- | 'ST' /α/ proxy value.
anStOf ∷ Proxy α → Proxy (ST α)
anStOf _ = Proxy

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
-- | 'STRef' proxy value.
anStRef ∷ Proxy STRef
anStRef = Proxy
#endif

-- | 'STRef' /α/ proxy value.
anStRefOf ∷ Proxy α → Proxy (STRef α)
anStRefOf _ = Proxy
