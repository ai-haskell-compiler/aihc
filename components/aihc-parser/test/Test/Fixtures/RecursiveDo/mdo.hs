{-# LANGUAGE RecursiveDo #-}
module MDo where

f = mdo
  x <- return 1
  return x
