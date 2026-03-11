{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
{- Copyright   :  (c) Chris Kuklewicz 2007 -}
module Text.Regex.PCRE.String(
  -- ** Types
  Regex,
  MatchOffset,
  MatchLength,
  CompOption(CompOption),
  ExecOption(ExecOption),
  ReturnCode,
  WrapError,
  -- ** Miscellaneous
  unusedOffset,
  getVersion,
  -- ** Medium level API functions
  compile,
  execute,
  regexec,
  -- ** Constants for CompOption
  compBlank,
  compAnchored,
  compAutoCallout,
  compCaseless,
  compDollarEndOnly,
  compDotAll,
  compExtended,
  compExtra,
  compFirstLine,
  compMultiline,
  compNoAutoCapture,
  compUngreedy,
  compUTF8,
  compNoUTF8Check,
  -- ** Constants for ExecOption
  execBlank,
  execAnchored,
  execNotBOL,
  execNotEOL,
  execNotEmpty,
  execNoUTF8Check,
  execPartial
  ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(fail))

import Text.Regex.PCRE.Wrap -- all
import Foreign.C.String(withCStringLen,withCString)
import Data.Array(Array,listArray)
import System.IO.Unsafe(unsafePerformIO)
import Text.Regex.Base.RegexLike(RegexMaker(..),RegexLike(..),RegexContext(..),MatchLength,MatchOffset)
import Text.Regex.Base.Impl(polymatch,polymatchM)

instance RegexContext Regex String String where
  match = polymatch
  matchM = polymatchM

unwrap :: (Show e) => Either e v -> IO v
unwrap x = case x of Left err -> fail ("Text.Regex.PCRE.String died: "++ show err)
                     Right v -> return v

instance RegexMaker Regex CompOption ExecOption String where
  makeRegexOpts c e pattern = unsafePerformIO $
    compile c e pattern >>= unwrap
  makeRegexOptsM c e pattern = either (fail.show) return $ unsafePerformIO $
    compile c e pattern

instance RegexLike Regex String where
  matchTest regex str = unsafePerformIO $
    withCStringLen str (wrapTest 0 regex) >>= unwrap
  matchOnce regex str = unsafePerformIO $
    execute regex str >>= unwrap
  matchAll regex str = unsafePerformIO $ 
    withCStringLen str (wrapMatchAll regex) >>= unwrap
  matchCount regex str = unsafePerformIO $ 
    withCStringLen str (wrapCount regex) >>= unwrap

-- | Compiles a regular expression
compile :: CompOption -- ^ Flags (summed together)
        -> ExecOption -- ^ Flags (summed together)
        -> String     -- ^ The regular expression to compile
        -> IO (Either (MatchOffset,String) Regex) -- ^ Returns: an error string and offset or the compiled regular expression
compile c e pattern = withCString pattern (wrapCompile c e)

-- | Matches a regular expression against a string
execute :: Regex      -- ^ Compiled regular expression
        -> String     -- ^ String to match against
        -> IO (Either WrapError (Maybe (Array Int (MatchOffset,MatchLength))))
                -- ^ Returns: 'Nothing' if the regex did not match the
                -- string, or:
                --   'Just' an array of (offset,length) pairs where index 0 is whole match, and the rest are the captured subexpressions.
execute regex str = do
  maybeStartEnd <- withCStringLen str (wrapMatch 0 regex)
  case maybeStartEnd of
    Right Nothing -> return (Right Nothing)
--  Right (Just []) -> fail "got [] back!" -- should never happen
    Right (Just parts) -> 
      return . Right . Just . listArray (0,pred (length parts))
      . map (\(s,e)->(fromIntegral s, fromIntegral (e-s))) $ parts
    Left err -> return (Left err)

-- | execute match and extract substrings rather than just offsets
regexec  :: Regex      -- ^ compiled regular expression
         -> String     -- ^ string to match
         -> IO (Either WrapError (Maybe (String, String,String, [String])))
                      -- ^ Returns: Nothing if no match, else
                      --   (text before match, text after match, array of matches with 0 being the whole match)
regexec regex str = do
  let getSub (start,stop) | start == unusedOffset = ""
                          | otherwise = take (stop-start) . drop start $ str
      matchedParts [] = ("","",str,[]) -- no information
      matchedParts (matchedStartStop@(start,stop):subStartStop) = 
        (take start str
        ,getSub matchedStartStop
        ,drop stop str
        ,map getSub subStartStop)
  maybeStartEnd <- withCStringLen str (wrapMatch 0 regex)
  case maybeStartEnd of
    Right Nothing -> return (Right Nothing)
--  Right (Just []) -> fail "got [] back!" -- should never happen
    Right (Just parts) -> return . Right . Just . matchedParts $ parts
    Left err -> return (Left err)
