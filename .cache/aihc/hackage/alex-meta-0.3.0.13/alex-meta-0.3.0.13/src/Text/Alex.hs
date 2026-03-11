-- -----------------------------------------------------------------------------
-- 
-- Alex.hs, part of Alex
--
-- (c) Chris Dornan 1995-2000, Simon Marlow 2003
--
-- ----------------------------------------------------------------------------}

module Text.Alex (
    runAlex
  , CLIFlags(..)
  , alex
  , optsToInject
  , importsToInject
  
  , parseScript, Target(..)
  ) where

import AbsSyn
import CharSet
import DFA
import DFAMin
import NFA
import Info
import Map ( Map )
import qualified Map hiding ( Map )
import Output
import ParseMonad ( runP )
import Parser
import Scan

import Data.Char ( chr )



runAlex :: [CLIFlags] -> Maybe FilePath -> String -> (String,String)
runAlex cli file prg =
  let script = parseScript file prg in
  alex cli script

parseScript :: Maybe FilePath -> String
  -> (Maybe (AlexPosn,Code), [Directive], Scanner, Maybe (AlexPosn,Code))
parseScript maybeFile prg =
  let file = maybe "<no file>" id maybeFile in
  case runP prg initialParserEnv parse of
        Left (Just (AlexPn _ line col),err) -> 
                error (file ++ ":" ++ show line ++ ":" ++ show col
                                 ++ ": " ++ err ++ "\n")
        Left (Nothing, err) ->
                error (file ++ ": " ++ err ++ "\n")

        Right script -> script



alex :: [CLIFlags]
     -> (Maybe (AlexPosn, Code), [Directive], Scanner, Maybe (AlexPosn, Code))
     -> (String,String)
alex cli script =
  let 
    target = if OptGhcTarget `elem` cli then GhcTarget else HaskellTarget
    encoding
      | OptLatin1 `elem` cli = Latin1   
      | otherwise            = UTF8
    (maybe_header, directives, scanner1, maybe_footer) = script
    (scanner2, scs, sc_hdr) = encodeStartCodes scanner1
    (scanner_final, actions) = extractActions scanner2
    dfa = scanner2dfa encoding scanner_final scs
    min_dfa = minimizeDFA dfa
    nm  = scannerName scanner_final
  in
   (maybe id ((++) . snd) (maybe_header) $ 
     maybe id (flip (++) . snd) (maybe_footer) $ 
     outputDFA target 1 nm min_dfa "" ++ (actions "") ++ (sc_hdr "")
      ,(infoDFA 1 nm min_dfa ""))

optsToInject :: Target -> [CLIFlags] -> String
optsToInject GhcTarget _ = "{-# OPTIONS -fglasgow-exts -cpp #-}\n"
optsToInject _         _ = "{-# OPTIONS -cpp #-}\n"

importsToInject :: Target -> [CLIFlags] -> String
importsToInject _ cli = always_imports ++ debug_imports ++ glaexts_import
  where
        glaexts_import | OptGhcTarget `elem` cli    = import_glaexts
                       | otherwise                  = ""

        debug_imports  | OptDebugParser `elem` cli = import_debug
                       | otherwise                 = ""

-- CPP is turned on for -fglasogw-exts, so we can use conditional
-- compilation.  We need to #include "config.h" to get hold of
-- WORDS_BIGENDIAN (see GenericTemplate.hs).

always_imports :: String
always_imports = "#if __GLASGOW_HASKELL__ >= 603\n" ++
                 "#include \"ghcconfig.h\"\n" ++
                 "#elif defined(__GLASGOW_HASKELL__)\n" ++
                 "#include \"config.h\"\n" ++
                 "#endif\n" ++
                 "#if __GLASGOW_HASKELL__ >= 503\n" ++
                 "import Data.Array\n" ++
                 "import Data.Char (ord)\n" ++
                 "import Data.Array.Base (unsafeAt)\n" ++
                 "#else\n" ++
                 "import Array\n" ++
                 "import Char (ord)\n" ++
                 "#endif\n"

import_glaexts :: String
import_glaexts = "#if __GLASGOW_HASKELL__ >= 503\n" ++
                 "import GHC.Exts\n" ++
                 "#else\n" ++
                 "import GlaExts\n" ++
                 "#endif\n"

import_debug :: String 
import_debug   = "#if __GLASGOW_HASKELL__ >= 503\n" ++
                 "import System.IO\n" ++
                 "import System.IO.Unsafe\n" ++
                 "import Debug.Trace\n" ++
                 "#else\n" ++
                 "import IO\n" ++
                 "import IOExts\n" ++
                 "#endif\n"

initialParserEnv :: (Map String CharSet, Map String RExp)
initialParserEnv = (initSetEnv, initREEnv)

initSetEnv :: Map String CharSet
initSetEnv = Map.fromList [("white", charSet " \t\n\v\f\r"),
                           ("printable", charSetRange (chr 32) (chr 0x10FFFF)), -- FIXME: Look it up the unicode standard
                           (".", charSetComplement emptyCharSet 
                            `charSetMinus` charSetSingleton '\n')]

initREEnv :: Map String RExp
initREEnv = Map.empty

-- -----------------------------------------------------------------------------
-- Command-line flags

data CLIFlags 
  = OptDebugParser
  | OptGhcTarget
  | OptInfoFile (Maybe FilePath)
  | OptLatin1
  | DumpHelp
  | DumpVersion
  deriving Eq

