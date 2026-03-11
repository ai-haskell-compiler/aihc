{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -ddump-rule-rewrites #-}

import Data.Word (Word8)

import Control.Lens (AsEmpty(..), Cons(..), prism, view)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Lens.Cons.Extras

main :: IO ()
main = defaultMain $ testGroup "Properties"
  [ testProperty "[a] -> [a]" (\s -> view recons (s :: String) == s)
  , testProperty "T -> T" (\s -> view recons (s :: T.Text) == s)
  , testProperty "TL -> TL" (\s -> view recons (s :: TL.Text) == s)
  , testProperty "B -> B" (\s -> view recons (s :: B.ByteString) == s)
  , testProperty "L -> L" (\s -> view recons (s :: L.ByteString) == s)
  , testProperty "Str -> T -> Str" (\s -> view recons (view recons (s :: String) :: T.Text) == s)
  , testProperty "Str -> TL -> Str" (\s -> view recons (view recons (s :: String) :: TL.Text) == s)
  , testProperty "T -> TL -> T" (\s -> view recons (view recons (s :: T.Text) :: TL.Text) == s)
  , testProperty "[W8] -> B -> [W8]" (\s -> view recons (view recons (s :: [Word8]) :: B.ByteString) == s)
  , testProperty "[W8] -> L -> [W8]" (\s -> view recons (view recons (s :: [Word8]) :: L.ByteString) == s)
  , testProperty "B -> L -> B" (\s -> view recons (view recons (s :: B.ByteString) :: L.ByteString) == s)
  , testProperty "[a] -> List a -> [a]" (\s -> view recons (view recons (s :: [Int]) :: List Int) == s)
  ]

-- Let's implement a type *without* rewrite rules to ensure that the
-- default implementation is correct
--
data List a = Nil | Cons a (List a)
  deriving (Eq)

instance AsEmpty (List a) where
  _Empty = prism (const Nil) f
    where
      f Nil = Right ()
      f xs = Left xs

instance Cons (List a) (List b) a b where
  _Cons = prism (uncurry Cons) f
    where
      f Nil = Left Nil
      f (Cons x xs) = Right (x, xs)
