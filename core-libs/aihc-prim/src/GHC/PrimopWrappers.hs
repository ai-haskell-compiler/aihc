{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Ordinary functions that call the primitive operations of "GHC.Prim".
-- GHC gives a wrapper for each primitive operation. This module gives the
-- wrappers of the 'Int#' arithmetic and comparisons. Like the wrappers of
-- GHC, they have no fixity declarations.
module GHC.PrimopWrappers
  ( (+#),
    (-#),
    (*#),
    negateInt#,
    (==#),
    (/=#),
    (<#),
    (<=#),
    (>#),
    (>=#),
  )
where

import GHC.Prim (Int#)
import GHC.Prim qualified as P

(+#) :: Int# -> Int# -> Int#
(+#) a b = a P.+# b

(-#) :: Int# -> Int# -> Int#
(-#) a b = a P.-# b

(*#) :: Int# -> Int# -> Int#
(*#) a b = a P.*# b

negateInt# :: Int# -> Int#
negateInt# = P.negateInt#

(==#) :: Int# -> Int# -> Int#
(==#) a b = a P.==# b

(/=#) :: Int# -> Int# -> Int#
(/=#) a b = a P./=# b

(<#) :: Int# -> Int# -> Int#
(<#) a b = a P.<# b

(<=#) :: Int# -> Int# -> Int#
(<=#) a b = a P.<=# b

(>#) :: Int# -> Int# -> Int#
(>#) a b = a P.># b

(>=#) :: Int# -> Int# -> Int#
(>=#) a b = a P.>=# b
