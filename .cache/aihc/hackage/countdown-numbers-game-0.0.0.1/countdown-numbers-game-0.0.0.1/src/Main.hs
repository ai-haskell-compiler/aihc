module Main where

import qualified Solve

import qualified Options.Applicative as OP



options :: OP.Parser ([Integer], Integer)
options =
   OP.liftA2 (,)
      (OP.many $
       OP.argument OP.auto
         (OP.help "given operand" <> OP.metavar "INTEGER"))
      (OP.option OP.auto
         (OP.help "wanted result" <> OP.long "result" <> OP.metavar "INTEGER"))

description :: OP.InfoMod a
description =
   OP.fullDesc
   <>
   OP.progDesc "Solve problems from the number round of the Countdown game show"

main :: IO ()
main = do
   problem <- OP.execParser $ OP.info (OP.helper <*> options) description
   mapM_ (putStrLn . Solve.format) $ Solve.run problem
