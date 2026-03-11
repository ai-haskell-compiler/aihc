{-# LANGUAGE CPP         #-}
#if MIN_VERSION_base(4,17,0)
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module GhcList (
    build,
) where

#if MIN_VERSION_base(4,17,0)
import GHC.List (build)
#else
import GHC.Exts (build)
#endif
