{-# LANGUAGE BlockArguments #-}
module BasicCase where

f x = id case x of
  _ -> ()
