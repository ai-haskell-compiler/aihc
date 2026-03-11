{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The test works if it builds!
module Main where

import Control.Monad
import Language.Haskell.TH.ReifyMany

class C a b where
   method :: b -> a

data A = A B (Maybe B)

data B = B Int

-- Tests support of type synonym instances.
type B' = B

instance C B' Int where method _ = B 0

$(do ns <- reifyManyWithoutInstances ''C [''A] (const True)
     when (ns /= [''A, ''Maybe]) $ fail "Didn't get expected list of datatypes."
     return []
 )

main :: IO ()
main = putStrLn "worked!"
