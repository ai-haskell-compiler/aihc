-- | Constants of @sys/mman.h@. WASI has no true @mmap@, and the header of
-- the sysroot stops with an error unless the compiler gives the macro of the
-- emulation. Some packages, such as @memory@, include the header only for
-- these constants.
module MemoryMap (readWrite) where

#include <sys/mman.h>

readWrite :: Int
readWrite = #const (PROT_READ | PROT_WRITE)
