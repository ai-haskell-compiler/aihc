{-# LANGUAGE CPP #-}
-- |
-- Module      : System.Arch
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module System.Arch
    ( Arch(..)
    , getSystemArch
    ) where

-- | List of all cpu architecture
data Arch = X86
          | X86_64
          | PPC
          | PPC64
          | Sparc
          | Arm
          | Mips
          | SH
          | IA64
          | S390
          | Alpha
          | Hppa
          | Rs6000
          | M68K
          | VAX
          deriving (Show,Eq)

-- | Return the system's cpu architecture
getSystemArch :: Arch
#if defined(ARCH_X86)
getSystemArch = X86
#elif defined(ARCH_X86_64)
getSystemArch = X86_64
#elif defined(ARCH_PPC)
getSystemArch = PPC
#elif defined(ARCH_PPC64)
getSystemArch = PPC64
#elif defined(ARCH_SPARC)
getSystemArch = Sparc
#elif defined(ARCH_ARM)
getSystemArch = Arm
#elif defined(ARCH_MIPS)
getSystemArch = Mips
#elif defined(ARCH_SH)
getSystemArch = SH
#elif defined(ARCH_IA64)
getSystemArch = IA64
#elif defined(ARCH_S390)
getSystemArch = S390
#elif defined(ARCH_ALPHA)
getSystemArch = Alpha
#elif defined(ARCH_HPPA)
getSystemArch = Hppa
#elif defined(ARCH_RS6000)
getSystemArch = Rs6000
#elif defined(ARCH_M68K)
getSystemArch = M68K
#elif defined(ARCH_VAX)
getSystemArch = VAX
#else
getSystemArch = undefined
#endif
