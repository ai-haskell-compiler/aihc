module Main (main) where

import Cpp (cppFile, headerFile, headerLocation)
import Dependency (dependencyFile, dependencyLocation)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)

location :: HasCallStack => String
location = prettyCallStack callStack

-- | A monad whose 'fail' keeps the message, so that the program can show
-- the message of a failed pattern bind.
newtype Check a = Check (Either String a)

instance Functor Check where
  fmap f (Check value) = Check (fmap f value)

instance Applicative Check where
  pure = Check . Right
  Check f <*> Check value = Check (f <*> value)

instance Monad Check where
  Check value >>= f = Check (value >>= \x -> let Check result = f x in result)

instance MonadFail Check where
  fail = Check . Left

failedBind :: Check Int
failedBind = do
  Just value <- pure Nothing
  pure value

main :: IO ()
main = do
  putStrLn location
  putStrLn dependencyLocation
  putStrLn dependencyFile
  putStrLn cppFile
  putStrLn headerFile
  putStrLn headerLocation
  let Check result = failedBind
  putStrLn (either id show result)
