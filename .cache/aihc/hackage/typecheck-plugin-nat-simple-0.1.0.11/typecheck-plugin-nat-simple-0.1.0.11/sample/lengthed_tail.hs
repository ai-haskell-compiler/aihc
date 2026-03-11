{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

import GHC.TypeNats

main :: IO ()
main = print . tail_ $ 1 :. 2 :. 3 :. Nil

infixr 6 :.

data List :: Nat -> * -> * where
	Nil :: List 0 a
	(:.) :: a -> List ln a -> List (ln + 1) a

deriving instance Show a => Show (List n a)

tail_ :: List (n + 1) a -> List n a
tail_ Nil = error "tail_: Nil"
tail_ (_ :. xs) = xs
