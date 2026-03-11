module Arch (
  Arch(..),
  allArchs,
  eitherArch,
  showArch,
  getSystemArch
  )
where

import Data.List.Extra (lower)
import SimpleCmd (cmd, error')

data Arch = Source
          | X86_64
          | AARCH64
          | ARMV7HL
          | PPC64LE
          | S390X
          | I386
          | RISCV64
  deriving Eq

allArchs :: [Arch]
allArchs = [X86_64, AARCH64, PPC64LE, S390X]

eitherArch :: String -> Either String Arch
eitherArch s =
  case lower s of
    "source" -> Right Source
    "x86_64" -> Right X86_64
    "aarch64" -> Right AARCH64
    "armv7hl" -> Right ARMV7HL
    "s390x" -> Right S390X
    "ppc64le" -> Right PPC64LE
    "i386" -> Right I386
    "riscv64" -> Right RISCV64
    _ -> Left $ "unknown arch: " ++ s

readArch :: String -> Arch
readArch =
  either error' id . eitherArch

showArch :: Arch -> String
showArch Source = "source"
showArch X86_64 = "x86_64"
showArch AARCH64 = "aarch64"
showArch ARMV7HL = "armv7hl"
showArch S390X = "s390x"
showArch PPC64LE = "ppc64le"
showArch I386 = "i386"
showArch RISCV64 = "riscv64"

getSystemArch :: IO Arch
getSystemArch =
  readArch <$> cmd "rpm" ["--eval", "%{_arch}"]
