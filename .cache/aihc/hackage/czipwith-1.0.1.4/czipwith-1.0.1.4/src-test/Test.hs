{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where



import Data.CZipWith
import Data.Functor.Identity
import Data.Functor.Const



data A f = A
  { a_str :: f String
  , a_bool :: f Bool
  }

data B f = B
  { b_int :: f Int
  , b_float :: f Float
  , b_a :: A f
  }

deriving instance Show (A Identity)
deriving instance Eq (A Identity)
deriving instance Eq (A (Const Bool))
deriving instance Eq (A Maybe)
deriving instance Eq (B Identity)
deriving instance Eq (B (Const Bool))
deriving instance Eq (B Maybe)


deriveCZipWith ''A
deriveCZipWith ''B

deriveCPointed ''A
deriveCPointed ''B

deriveCZipWithM ''A
deriveCZipWithM ''B

main :: IO ()
main = do
  let x1 =
        B (Identity 12) (Identity 3.1) (A (Identity "string") (Identity True))
  let x2 = B (Just 1) Nothing (A (Just "just") Nothing)
  let x3 = cZipWith
        (\x my -> case my of
          Nothing -> x
          Just y  -> Identity y
        )
        x1
        x2
  errorIf
      (x3 /= B (Identity 1) (Identity 3.1) (A (Identity "just") (Identity True))
      )
    $ return ()
  let (Identity x4) = cZipWithM
        (\x my -> Identity $ case my of
          Nothing -> x
          Just y  -> Identity y
        )
        x1
        x2
  errorIf
      (x4 /= B (Identity 1) (Identity 3.1) (A (Identity "just") (Identity True))
      )
    $ return ()
  let (Identity x5) = cTraverse Identity x2
  errorIf (x2 /= x5) $ return ()
  let x6 = cPoint (Const True)
  errorIf (x6 /= B (Const True) (Const True) (A (Const True) (Const True)))
    $ return ()
  putStrLn "no errors found!"

errorIf :: Bool -> a -> a
errorIf False = id
errorIf True  = error "errorIf"
