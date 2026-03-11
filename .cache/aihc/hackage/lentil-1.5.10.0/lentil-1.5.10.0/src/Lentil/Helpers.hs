-----------------------------------------------------------------------------
-- |
-- Module      :  Lentil.Helpers
-- Copyright   :  © 2015 Francesco Ariis
-- License     :  GPLv3 (see the LICENSE file)
--
-- Ancillaries for other modules
-----------------------------------------------------------------------------

module Lentil.Helpers where

import Lentil.Types

import qualified System.IO            as I
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as PC
import qualified Control.Monad.State  as S
import qualified Data.Void            as V


type StateParser s a = P.ParsecT V.Void String (S.State s) a

-- output errors (to stderr)
perr :: String -> IO ()
perr cs  = I.hPutStrLn I.stderr cs

-------------
-- PARSING --
-------------

type StateParserError = P.ParseErrorBundle String V.Void
runStateParser :: StateParser s a -> s -> FilePath -> String ->
                  Either StateParserError a
runStateParser p s fp i =
                 let
                     eps = P.runParserT p fp i
                     ep  = fst $ S.runState eps s
                 in ep

-- parse an extension alias "aa->bc" -> Just ("aa", "bc")
aliasp :: String -> Maybe Alias
aliasp s = either (const Nothing) Just (runStateParser p () "" s)
    where
          p :: StateParser s (String, String)
          p = P.someTill P.anySingle (PC.string aliasSign) >>= \a ->
              P.many P.anySingle                           >>= \b ->
              return ('.':a, '.':b)

