module Hint.Parsers where

import Prelude hiding (span)

import Hint.Base

import Control.Monad.IO.Class (liftIO)

import qualified Hint.GHC as GHC

data ParseResult = ParseOk | ParseError GHC.SrcSpan GHC.Message

parseExpr :: MonadInterpreter m => String -> m ParseResult
parseExpr = runParser GHC.parseStmt

parseType :: MonadInterpreter m => String -> m ParseResult
parseType = runParser GHC.parseType

runParser :: MonadInterpreter m => GHC.P a -> String -> m ParseResult
runParser parser expr =
    do dyn_fl <- runGhc GHC.getSessionDynFlags
       --
       buf <- (return . GHC.stringToStringBuffer) expr
       --
       -- ghc >= 7 panics if noSrcLoc is given
       let srcLoc = GHC.mkRealSrcLoc (GHC.fsLit "<hint>") 1 1
       let parserOpts = GHC.mkParserOpts dyn_fl
       let parse_res = GHC.unP parser (GHC.initParserState parserOpts buf srcLoc)
       --
       case parse_res of
           GHC.POk{}            -> return ParseOk
           --
#if MIN_VERSION_ghc(8,10,0)
           GHC.PFailed pst      -> let errMsgs = GHC.getErrorMessages pst dyn_fl
                                       span = GHC.errMsgSpan errMsgs
                                       err = GHC.vcat $ GHC.pprErrorMessages errMsgs
                                   in pure (ParseError span err)
#else
           GHC.PFailed _ span err -> return (ParseError span err)
#endif

failOnParseError :: MonadInterpreter m
                 => (String -> m ParseResult)
                 -> String
                 -> m ()
failOnParseError parser expr = mayFail go
    where go = parser expr >>= \ case
                      ParseOk             -> return (Just ())
                      -- If there was a parsing error,
                      -- do the "standard" error reporting
                      ParseError span err ->
                          do -- parsing failed, so we report it just as all
                             -- other errors get reported....
                             logger <- fromSession ghcLogger
                             dflags <- runGhc GHC.getSessionDynFlags
                             let logger'  = GHC.putLogMsg logger dflags

                             liftIO $ logger'
                                              GHC.NoReason
                                              GHC.SevError
                                              span
                                              err
                             --
                             -- behave like the rest of the GHC API functions
                             -- do on error...
                             return Nothing
