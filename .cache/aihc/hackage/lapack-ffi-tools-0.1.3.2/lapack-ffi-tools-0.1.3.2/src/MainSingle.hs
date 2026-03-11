module Main where

import qualified Type
import Parser.Signature (Identifier(Identifier))
import CreateBinding
         (Parameter, formatForeignCall, foreignFromParameters, parseHeader)

import qualified Text.ParserCombinators.Parsec as Parsec

import qualified System.IO as IO

import qualified Control.Monad.Trans.Writer as MW
import Control.Applicative ((<$>))

import Data.Char (toLower)


foreignSig ::
   (Identifier, [Parameter], Maybe Type.Mono) ->
   ((String, String), Type.Foreign Type.Mono)
foreignSig (Identifier name, params, returnType) =
   ((map toLower name, name), foreignFromParameters params returnType)

main :: IO ()
main =
   either (IO.hPutStrLn IO.stderr . show) putStr .
   Parsec.parse
      (formatForeignCall . foreignSig . fst . MW.runWriter <$> parseHeader)
      "<stdin>"
      =<< getContents
