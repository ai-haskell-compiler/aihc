module Test.Tutorial.Exercise1 where

import Test.Tutorial.Application

newApp :: App
newApp =
  let logger' = newLogger
      console' = newConsole
      userInput' = newUserInput console'
      rng' = newRng logger'
      secretReader' = newSecretReader (SecretReaderConfig "test/Test/Tutorial/secret.txt") logger'
   in App
        { userInput = userInput',
          console = console',
          rng = rng',
          secretReader = secretReader'
        }
