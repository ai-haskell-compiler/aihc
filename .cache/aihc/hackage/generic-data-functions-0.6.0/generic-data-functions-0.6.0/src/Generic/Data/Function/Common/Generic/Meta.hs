{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- for my testing

module Generic.Data.Function.Common.Generic.Meta where

import GHC.Generics
import GHC.TypeLits
import GHC.Exts ( proxy# )

-- | List every constructor name in a generic type rep.
type family CstrNames gf :: [Symbol] where
    CstrNames (l :+: r) = CstrNames l ++ CstrNames r
    CstrNames (C1 (MetaCons n _ _) _) = '[n]

-- | Append for type-level lists.
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
    '[] ++ bs = bs
    (a ': as) ++ bs = a ': (as ++ bs)

-- | Reify a list of type-level strings. Order is maintained.
class KnownSymbols as where symbolVals :: [String]
instance (KnownSymbol a, KnownSymbols as) => KnownSymbols (a ': as) where
    symbolVals = symbolVal' (proxy# @a) : symbolVals @as
instance KnownSymbols '[] where symbolVals = []
