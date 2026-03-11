{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
This exports instances of the high level API and the medium level
API of 'compile','execute', and 'regexec'.
-}
{- Copyright   :  (c) Chris Kuklewicz 2007 -}
module Text.Regex.PCRE.ByteString.Lazy(
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
  -- ** CompOption flags
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
  -- ** ExecOption flags
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
import Data.Array(Array)
import qualified Data.ByteString.Lazy as L(ByteString,toChunks,fromChunks,last,null,snoc)
import qualified Data.ByteString as B(ByteString,concat,pack)
import qualified Data.ByteString.Unsafe as B(unsafeUseAsCString,unsafeUseAsCStringLen)
import System.IO.Unsafe(unsafePerformIO)
import Text.Regex.Base.RegexLike(RegexContext(..),RegexMaker(..),RegexLike(..),MatchOffset,MatchLength)
import Text.Regex.Base.Impl(polymatch,polymatchM)
import qualified Text.Regex.PCRE.ByteString as BS(execute,regexec)
import Foreign.C.String(CString,CStringLen)
import Foreign(nullPtr)

instance RegexContext Regex L.ByteString L.ByteString where
  match = polymatch
  matchM = polymatchM

{-# INLINE fromLazy #-}
fromLazy :: L.ByteString -> B.ByteString
fromLazy = B.concat . L.toChunks

{-# INLINE toLazy #-}
toLazy :: B.ByteString -> L.ByteString
toLazy = L.fromChunks . return

unwrap :: (Show e) => Either e v -> IO v
unwrap x = case x of Left err -> fail ("Text.Regex.PCRE.ByteString.Lazy died: "++ show err)
                     Right v -> return v

{-# INLINE asCString #-}
asCString :: L.ByteString -> (CString -> IO a) -> IO a
asCString s = if (not (L.null s)) && (0==L.last s)
                then B.unsafeUseAsCString (fromLazy s)
                else B.unsafeUseAsCString (fromLazy (L.snoc s 0))

{-# INLINE asCStringLen #-}
asCStringLen :: L.ByteString -> (CStringLen -> IO a) -> IO a
asCStringLen ls op = B.unsafeUseAsCStringLen (fromLazy ls) checked
  where checked cs@(ptr,_) | ptr == nullPtr = B.unsafeUseAsCStringLen myEmpty (op . trim)
                           | otherwise = op cs
        myEmpty = B.pack [0]
        trim (ptr,_) = (ptr,0)

instance RegexMaker Regex CompOption ExecOption L.ByteString where
  makeRegexOpts c e pattern = unsafePerformIO $
    compile c e pattern >>= unwrap
  makeRegexOptsM c e pattern = either (fail.show) return $ unsafePerformIO $
    compile c e pattern

instance RegexLike Regex L.ByteString where
  matchTest regex bs = unsafePerformIO $
    asCStringLen bs (wrapTest 0 regex) >>= unwrap
  matchOnce regex bs = unsafePerformIO $
    execute regex bs >>= unwrap
  matchAll regex bs = unsafePerformIO $
    asCStringLen bs (wrapMatchAll regex) >>= unwrap
  matchCount regex bs = unsafePerformIO $
    asCStringLen bs (wrapCount regex) >>= unwrap

-- ---------------------------------------------------------------------
-- | Compiles a regular expression
--
compile :: CompOption  -- ^ (summed together)
        -> ExecOption  -- ^ (summed together)
        -> L.ByteString  -- ^ The regular expression to compile
        -> IO (Either (MatchOffset,String) Regex) -- ^ Returns: the compiled regular expression
compile c e pattern = do
  asCString pattern (wrapCompile c e)

-- ---------------------------------------------------------------------
-- | Matches a regular expression against a buffer, returning the buffer
-- indicies of the match, and any submatches
--
-- | Matches a regular expression against a string
execute :: Regex      -- ^ Compiled regular expression
        -> L.ByteString -- ^ String to match against
        -> IO (Either WrapError (Maybe (Array Int (MatchOffset,MatchLength))))
                -- ^ Returns: 'Nothing' if the regex did not match the
                -- string, or:
                --   'Just' an array of (offset,length) pairs where index 0 is whole match, and the rest are the captured subexpressions.
execute regex bs = BS.execute regex (fromLazy bs)

regexec :: Regex      -- ^ Compiled regular expression
        -> L.ByteString -- ^ String to match against
        -> IO (Either WrapError (Maybe (L.ByteString, L.ByteString, L.ByteString, [L.ByteString])))
regexec regex bs = do
  x <- BS.regexec regex (fromLazy bs)
  return $ case x of
             Left e -> Left e
             Right Nothing -> Right Nothing
             Right (Just (a,b,c,ds)) -> Right (Just (toLazy a,toLazy b,toLazy c,map toLazy ds))
