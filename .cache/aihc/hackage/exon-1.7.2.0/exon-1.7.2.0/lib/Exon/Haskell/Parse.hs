{-# LANGUAGE CPP #-}

-- | This module is here to parse Haskell expression using the GHC Api
--
-- Stolen from ghc-hs-meta
module Exon.Haskell.Parse (parseExp, parseExpWithExts, parseExpWithFlags, parseHsExpr) where

import Prelude hiding (srcLoc)

#if MIN_VERSION_ghc(9,4,0)
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Annotation (LocatedA)
import GHC.Utils.Outputable (ppr, defaultSDocContext, renderWithContext)
#else
import qualified GHC.Parser.Errors.Ppr as ParserErrorPpr
import GHC.Parser.Annotation (LocatedA)
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Config.Parser (initParserOpts)
#else
import GHC.Driver.Config (initParserOpts)
#endif

import GHC.Parser.PostProcess
import qualified GHC.Types.SrcLoc as SrcLoc
import GHC.Driver.Session
import GHC.Data.StringBuffer
import GHC.Parser.Lexer
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Parser as Parser
import GHC.Data.FastString
import GHC.Types.SrcLoc

import GHC.Hs.Extension (GhcPs)

-- @HsExpr@ is available from GHC.Hs.Expr in all versions we support.
-- However, the goal of GHC is to split HsExpr into its own package, under
-- the namespace Language.Haskell.Syntax. The module split happened in 9.0,
-- but still in the ghc package.
import Language.Haskell.Syntax (HsExpr(..))

import Language.Haskell.TH (Extension(..))
import qualified Language.Haskell.TH.Syntax as TH

import qualified Exon.Haskell.Settings as Settings
import Exon.Haskell.Translate (toExp)

-- | Parse a Haskell expression from source code into a Template Haskell expression.
-- See @parseExpWithExts@ or @parseExpWithFlags@ for customizing with additional extensions and settings.
parseExp :: String -> Either (Int, Int, String) TH.Exp
#if MIN_VERSION_ghc(9,2,0)
parseExp = parseExpWithExts
    [ TypeApplications
    , OverloadedRecordDot
    , OverloadedLabels
    , OverloadedRecordUpdate
    ]
#else
parseExp = parseExpWithExts
    [ TypeApplications
    , OverloadedLabels
    ]
#endif

-- | Parse a Haskell expression from source code into a Template Haskell expression
-- using a given set of GHC extensions.
parseExpWithExts :: [Extension] -> String -> Either (Int, Int, String) TH.Exp
parseExpWithExts exts = parseExpWithFlags (Settings.baseDynFlags exts)

-- | Parse a Haskell expression from source code into a Template Haskell expression
-- using a given set of GHC DynFlags.
parseExpWithFlags :: DynFlags -> String -> Either (Int, Int, String) TH.Exp
parseExpWithFlags flags expStr = do
  hsExpr <- parseHsExpr flags expStr
  pure (toExp flags hsExpr)

-- | Run the GHC parser to parse a Haskell expression into a @HsExpr@.
parseHsExpr :: DynFlags -> String -> Either (Int, Int, String) (HsExpr GhcPs)
parseHsExpr dynFlags s =
  case runParser dynFlags s of
    POk _ locatedExpr ->
      let expr = SrcLoc.unLoc locatedExpr
       in Right
            expr

{- ORMOLU_DISABLE #-}
#if MIN_VERSION_ghc(9,4,0)
    PFailed PState{loc=SrcLoc.psRealLoc -> srcLoc, errors=errorMessages} ->
            let
                err = renderWithContext defaultSDocContext (ppr errorMessages)
                line = SrcLoc.srcLocLine srcLoc
                col = SrcLoc.srcLocCol srcLoc
            in Left (line, col, err)
#else
    PFailed PState{loc=SrcLoc.psRealLoc -> srcLoc, errors=errorMessages} ->
            let
                psErrToString e = show $ ParserErrorPpr.pprError e
                err = concatMap psErrToString errorMessages
                -- err = concatMap show errorMessages
                line = SrcLoc.srcLocLine srcLoc
                col = SrcLoc.srcLocCol srcLoc
            in Left (line, col, err)
#endif

-- From Language.Haskell.GhclibParserEx.GHC.Parser

parse :: P a -> String -> DynFlags -> ParseResult a
parse p str flags =
  Lexer.unP p parseState
  where
    location = mkRealSrcLoc (mkFastString "<string>") 1 1
    strBuffer = stringToStringBuffer str
    parseState =
      initParserState (initParserOpts flags) strBuffer location

runParser :: DynFlags -> String -> ParseResult (LocatedA (HsExpr GhcPs))
runParser flags str =
  case parse Parser.parseExpression str flags of
    POk s e -> unP (runPV (unECP e)) s
    PFailed ps -> PFailed ps
