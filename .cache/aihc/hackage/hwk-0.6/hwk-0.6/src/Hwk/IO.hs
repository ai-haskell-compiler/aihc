module Hwk.IO (execExpr, shellSession)
where

import Language.Haskell.Interpreter

import Common
import Hwk.Types

-- FIXME add --safe?
execExpr :: String -> Interpreter ()
execExpr stmt = do
  typ <- typeOf stmt
  case cleanupType typ of
    "IO ()" -> runStmt stmt
    "IO String" ->
      runStmt $ stmt ++ ">>= putStrLn"
    "IO [String]" ->
      runStmt $ stmt ++ ">>= mapM_ putStrLn"
    "IO [[String]]" ->
      runStmt $ stmt ++ ">>= mapM_ (putStrLn . unwords)"
    _ -> liftIO $ warn typ

shellSession :: Interpreter ()
shellSession = do
  liftIO $ putStr ">= "
  stmt <- liftIO getLine
  if null (words stmt) then shellSession
    else do
    etypchk <- typeChecksWithDetails stmt
    case etypchk of
      Left err -> do
        liftIO $ mapM_ (putStrLn . errMsg) err
        runStmt stmt
        shellSession
      Right typ -> do
        liftIO $ putStrLn $ ":: " ++ cleanupType typ
        case cleanupType typ of
          "IO ()" -> runStmt stmt
          "IO String" ->
            runStmt $ stmt ++ ">>= putStrLn"
          "IO [String]" ->
            runStmt $ stmt ++ ">>= mapM_ putStrLn"
          "IO [[String]]" ->
            runStmt $ stmt ++ ">>= mapM_ (putStrLn . unwords)"
          _ -> eval stmt >>= liftIO . putStrLn
        shellSession
