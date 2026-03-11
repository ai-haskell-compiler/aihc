module Test.Hspec.SmallCheck.Types where

import           Prelude ()
import           Test.Hspec.SmallCheck.Compat

import           Data.List
import           Test.Hspec.Core.Spec (Location(..))


data Result = Failure (Maybe Location) Reason
  deriving (Eq, Show, Read)

data Reason =
    Reason String
  | ExpectedActual String String String
  deriving (Eq, Show, Read)

parseResult :: String -> (String, Maybe Result)
parseResult xs =  case [(x, Just y) | (x, Just y) <- zip (inits xs) (map readMaybe $ tails xs)] of
  r : _ -> r
  [] -> (xs, Nothing)

concatPrefix :: String -> String -> Maybe String
concatPrefix a b = case filter (not . null) $ [a, b] of
  [] -> Nothing
  xs -> Just (intercalate "\n" xs)
