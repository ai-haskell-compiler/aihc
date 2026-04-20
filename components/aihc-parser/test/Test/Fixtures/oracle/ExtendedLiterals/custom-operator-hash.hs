{- ORACLE_TEST xfail ExtendedLiterals with custom operator -}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExtendedLiterals #-}

f64_predecessorIEEE#
  :: Double#
  -> Double#
f64_predecessorIEEE#
  value
  = symetric_result
  where
    symetric_result
      = negateDouble#
      $# f64_successorIEEE#
      $# negateDouble#
      $# value

    infixr 0 $#
    ($#) :: (Double# -> Double#) -> Double# -> Double#
    f $# x = f x
