{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Test.Tutorial.Application where

import qualified Data.ByteString.Char8 as BS8
import Protolude
import System.Directory (doesFileExist)
import System.Random

data App = App
  { userInput :: UserInput IO,
    secretReader :: SecretReader IO,
    rng :: Rng IO,
    console :: Console IO
  }

startApp :: App -> IO ()
startApp app@App {..} = do
  userAnswer <- askQuestion userInput
  continue <-
    case userAnswer of
      Maybe -> randomBool rng
      No -> pure False
      Yes -> pure True

  if continue
    then do
      secret <- readSecret secretReader
      case secret of
        Nothing -> write console "Sorry I actually don't know"
        Just s -> do
          write console ("the answer is " <> s)
          startApp app
    else write console "bye"

data Logger m = Logger
  { info :: Text -> m (),
    error :: Text -> m ()
  }

newLogger :: Logger IO
newLogger =
  Logger
    { info = \t -> print $ "[INFO] " <> t,
      error = \t -> print $ "[ERROR] " <> t
    }

data UserAnswer = Yes | No | Maybe deriving (Eq, Show)

data UserInput m = UserInput
  { askQuestion :: m UserAnswer
  }

newUserInput :: Console IO -> UserInput IO
newUserInput console =
  let userInput =
        UserInput
          { askQuestion = do
              write console "Do you want to know the answer to Life, the Universe and Everything? (Yes/No/Maybe)"
              answer <- read console
              case answer of
                "Yes" -> pure Yes
                "No" -> pure No
                "Maybe" -> pure Maybe
                _ -> write console "Please enter Yes, No or Maybe" >> askQuestion userInput
          }
   in userInput

data Rng m = Rng
  { randomBool :: m Bool
  }

newRng :: Logger IO -> Rng IO
newRng logger =
  Rng
    { randomBool = do
        b <- randomIO
        info logger ("generated a random boolean " <> show b)
        pure b
    }

-- Get the secret answer and return Nothing if not found
data SecretReader m = SecretReader
  { readSecret :: m (Maybe Text)
  }

data SecretReaderConfig = SecretReaderConfig Text deriving (Eq, Show)

newSecretReader :: SecretReaderConfig -> Logger IO -> SecretReader IO
newSecretReader (SecretReaderConfig path) logger =
  SecretReader
    { readSecret = do
        exists <- doesFileExist (toS path)
        if exists
          then Just . decodeUtf8 <$> BS8.readFile (toS path)
          else error logger ("file does not exist at " <> path) $> Nothing
    }

data Console m = Console
  { write :: Text -> m (),
    read :: m Text
  }

newConsole :: Console IO
newConsole = Console putStrLn (toS <$> getLine)
