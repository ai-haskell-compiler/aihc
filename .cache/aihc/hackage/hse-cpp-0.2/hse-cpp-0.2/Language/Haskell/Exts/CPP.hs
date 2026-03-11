module Language.Haskell.Exts.CPP
  ( parseFileWithCommentsAndCPP
  , parseFileContentsWithCommentsAndCPP
  , defaultCpphsOptions
  , CpphsOptions(..)
  , BoolOptions(..)
  ) where

import qualified Language.Preprocessor.Cpphs as Orig
import Language.Preprocessor.Cpphs hiding (defaultCpphsOptions)
import Language.Preprocessor.Unlit
import Language.Haskell.Exts
import Control.Applicative
import Data.List

parseFileWithCommentsAndCPP ::  CpphsOptions -> ParseMode -> FilePath
                      -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseFileWithCommentsAndCPP cppopts parseMode0 file = do
    content <- readFile file
    parseFileContentsWithCommentsAndCPP cppopts parseMode content
  where
    parseMode = parseMode0 { parseFilename = file }

parseFileContentsWithCommentsAndCPP
    :: CpphsOptions -> ParseMode -> String
    -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseFileContentsWithCommentsAndCPP cppopts p rawStr = do
    let filename = parseFilename p
        md = delit filename rawStr
        cppMode = updateExtensions p md
    processedSrc <- cpp cppopts cppMode md
    let finalMode = updateExtensions cppMode processedSrc
    return $ parseModuleWithComments finalMode processedSrc

updateExtensions :: ParseMode -> String -> ParseMode
updateExtensions p mod =
  let oldLang = baseLanguage p
      exts = extensions p
      (bLang, extraExts) =
          case (ignoreLanguagePragmas p, readExtensions mod) of
            (False, Just (mLang, es)) ->
                 (case mLang of {Nothing -> oldLang;Just newLang -> newLang}, es)
            _ -> (oldLang, [])
  in p { extensions = exts ++ extraExts
       , ignoreLanguagePragmas = False
       , baseLanguage = bLang
       }

cpp cppopts p str
  | CPP `elem` impliesExts (toExtensionList (baseLanguage p) (extensions p))
  = runCpphs cppopts (parseFilename p) str
  | otherwise = return str

delit :: String -> String -> String
delit fn = if ".lhs" `isSuffixOf` fn then unlit fn else id

defaultCpphsOptions =
  Orig.defaultCpphsOptions
  { boolopts = (boolopts Orig.defaultCpphsOptions)
      { locations = True
      , stripC89 = True
      , stripEol = False
      , hashline = False
      }
  }
