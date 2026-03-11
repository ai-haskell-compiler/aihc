#include "Rts.h"

/**
 * Rewrite of some Cmm in C. It is not in Cmm because `bdescr_flags` is not available
 * until this commit:
 * https://gitlab.haskell.org/ghc/ghc/commit/310371ff2d5b73cdcb2439b67170ca5e613541c0
 *
 * Cmm version can be found here:
 * https://gitlab.haskell.org/ghc/ghc/blob/4e8a71c1138b587dfbab8a1823b3f7fa6f0166bd/rts/PrimOps.cmm#L157-174
 *
 * Its types in Haskell are:
 * - `ByteArray# s -> Int#`
 * - `MutableByteArray# s -> Int#`
 *
 */
long pvar_is_byte_array_pinned(StgPtr ba)
{
  bdescr *bd = Bdescr(ba);
  // All of BF_PINNED, BF_LARGE and BF_COMPACT are considered immovable. Although
  // BF_COMPACT is only available in ghc-8.2, so we don't care about it.
  return ((bd->flags & (BF_PINNED | BF_LARGE)) != 0);
}
