module Aihc.Cli.Repl
  ( ReplStep (..),
    evaluateExpression,
    handleReplInput,
    runRepl,
  )
where

import Data.Char (isSpace)
import System.Console.Haskeline qualified as Haskeline

data ReplStep
  = ReplContinue !(Maybe String)
  | ReplExit !(Maybe String)
  deriving (Eq, Show)

runRepl :: IO ()
runRepl =
  Haskeline.runInputT Haskeline.defaultSettings loop
  where
    loop = do
      minput <- Haskeline.getInputLine "aihc> "
      case minput of
        Nothing -> pure ()
        Just input ->
          case handleReplInput input of
            ReplContinue output -> do
              mapM_ Haskeline.outputStrLn output
              loop
            ReplExit output ->
              mapM_ Haskeline.outputStrLn output

handleReplInput :: String -> ReplStep
handleReplInput input =
  let trimmedInput = trim input
   in case trimmedInput of
        "" -> ReplContinue Nothing
        ":quit" -> ReplExit Nothing
        ":q" -> ReplExit Nothing
        ":help" -> ReplContinue (Just helpText)
        ":?" -> ReplContinue (Just helpText)
        command@(':' : _) -> ReplContinue (Just ("unknown command: " <> command))
        _ -> ReplContinue (Just (evaluateExpression trimmedInput))

evaluateExpression :: String -> String
evaluateExpression _ = "unimplemented"

helpText :: String
helpText =
  unlines
    [ "Commands:",
      "  :quit, :q  Exit the REPL",
      "  :help, :?  Show this help"
    ]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate = reverse . dropWhile predicate . reverse
