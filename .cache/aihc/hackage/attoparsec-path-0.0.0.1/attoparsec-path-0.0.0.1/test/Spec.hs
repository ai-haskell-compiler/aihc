{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where

import Data.Attoparsec.Text
import Data.Attoparsec.Path
import qualified Data.Text as T
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Instances
import Unsafe.Coerce

newtype RelFile = RelFile String
  deriving (Show)

instance Arbitrary RelFile where
  arbitrary = do
    xs <- arbitrary
    pure $ RelFile $
      if T.length xs > 0
      then  let xs' = if T.last xs == '/'
                      then xs <> "a"
                      else xs
                xs'' = if T.head xs' == '/'
                       then "a" <> xs'
                       else xs'
            in  T.unpack xs''
      else "foo"

newtype AbsFile = AbsFile String
  deriving (Show)

instance Arbitrary AbsFile where
  arbitrary = do
    RelFile xs <- arbitrary
    pure $ AbsFile $ "/" ++ xs

newtype RelDir = RelDir String
  deriving (Show)

instance Arbitrary RelDir where
  arbitrary = do
    RelFile xs <- arbitrary
    pure $ RelDir $ xs ++ "/"

newtype AbsDir = AbsDir String
  deriving (Show)

instance Arbitrary AbsDir where
  arbitrary = do
    RelDir xs <- arbitrary
    pure $ AbsDir $ "/" ++ xs



main :: IO ()
main = do
  quickCheck $ \(RelFile x) -> parseOnly (relFilePath <* endOfInput) (T.pack x) == Right (unsafeCoerce x)
  quickCheck $ \(AbsFile x) -> parseOnly (absFilePath <* endOfInput) (T.pack x) == Right (unsafeCoerce x)
  quickCheck $ \(RelDir x) -> parseOnly (relDirPath <* endOfInput) (T.pack x) == Right (unsafeCoerce x)
  quickCheck $ \(AbsDir x) -> parseOnly (absDirPath <* endOfInput) (T.pack x) == Right (unsafeCoerce x)
  quickCheck $ \(RelFile x) -> case parseOnly (relFilePath <* endOfInput) (T.pack x <> "/") of
    Left _ -> True
    _      -> False
  quickCheck $ \(RelFile x) -> case parseOnly (absFilePath <* endOfInput) (T.pack x <> "/") of
    Left _ -> True
    _      -> False
  quickCheck $ \(RelFile x) -> case parseOnly (relDirPath <* endOfInput) (T.pack x) of
    Left _ -> True
    _      -> False
  quickCheck $ \(RelFile x) -> case parseOnly (absDirPath <* endOfInput) (T.pack x) of
    Left _ -> True
    _      -> False
