{-# OPTIONS_GHC -fno-warn-orphans          #-}

-- |
-- Copyright: (c) 2006-2017, Chris Dornan and Christopher Kuklewicz
-- SPDX-License-Identifier: BSD-3-Clause
--
-- This exports instances of the high level API and the medium level API of 'compile','execute', and 'regexec'.
--
-- @since 0.95.0
module Text.Regex.PCRE.Text.Lazy
  (
  -- * Types
    Regex
  , MatchOffset
  , MatchLength
  , CompOption(CompOption)
  , ExecOption(ExecOption)
  , ReturnCode
  , WrapError
  -- * Miscellaneous
  , unusedOffset
  , getVersion
  -- * Medium level API functions
  , compile
  , execute
  , regexec
  -- * CompOption flags
  , compBlank
  , compAnchored
  , compAutoCallout
  , compCaseless
  , compDollarEndOnly
  , compDotAll
  , compExtended
  , compExtra
  , compFirstLine
  , compMultiline
  , compNoAutoCapture
  , compUngreedy
  , compUTF8
  , compNoUTF8Check
  -- * ExecOption flags
  , execBlank
  , execAnchored
  , execNotBOL
  , execNotEOL
  , execNotEmpty
  , execNoUTF8Check
  , execPartial
  ) where

import           Data.Array(Array,listArray)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Unsafe       as B
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy               as TL
import           Foreign.C.String(CStringLen,CString)
import           Foreign(nullPtr)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Regex.Base.Impl
import           Text.Regex.Base.RegexLike
import           Text.Regex.PCRE.Wrap


instance RegexContext Regex TL.Text TL.Text where
  match   = polymatch
  matchM  = polymatchM

instance RegexMaker Regex CompOption ExecOption TL.Text where
  makeRegexOpts c e pat = unsafePerformIO $
    compile c e pat >>= unwrap

  makeRegexOptsM c e pat = either (fail.show) return $ unsafePerformIO $
    compile c e pat

instance RegexLike Regex TL.Text where
  matchTest re tx = unsafePerformIO $
    asCStringLen tx (wrapTest 0 re) >>= unwrap

  matchOnce re tx = unsafePerformIO $
    execute re tx >>= unwrap

  matchAll re tx = unsafePerformIO $
    asCStringLen tx (wrapMatchAll re) >>= unwrap

  matchCount re tx = unsafePerformIO $
    asCStringLen tx (wrapCount re) >>= unwrap


-- ---------------------------------------------------------------------
-- | Compiles a regular expression
compile :: CompOption   -- ^ (summed together)
        -> ExecOption   -- ^ (summed together)
        -> TL.Text      -- ^ The regular expression to compile
        -> IO (Either (MatchOffset,String) Regex) -- ^ Returns: the compiled regular expression
compile c e pat =
  -- PCRE does not allow one to specify a length for the regular expression, it must by 0 terminated
  asCString pat $ wrapCompile c e


-- ---------------------------------------------------------------------
-- | Matches a regular expression against a string
execute :: Regex      -- ^ Compiled regular expression
        -> TL.Text    -- ^ Text to match against
        -> IO (Either WrapError (Maybe (Array Int (MatchOffset,MatchLength))))
                -- ^ Returns: 'Nothing' if the regex did not match the
                -- string, or 'Just' an array of (offset,length) pairs
                -- where index 0 is whole match, and the rest are the
                -- captured subexpressions.
execute re tx = do
  maybeStartEnd <- asCStringLen tx $ wrapMatch 0 re
  case maybeStartEnd of
    Right Nothing      -> return $ Right Nothing
    Right (Just parts) ->
      return $ Right $ Just $ listArray (0,pred $ length parts)
          [ (s,e-s) | (s,e) <- parts ]
    Left err           -> return $ Left err

-- ---------------------------------------------------------------------
-- | Matches a regular expression against a string
regexec :: Regex      -- ^ Compiled regular expression
        -> TL.Text    -- ^ Text to match against
        -> IO (Either WrapError (Maybe (TL.Text, TL.Text, TL.Text, [TL.Text])))
                -- ^ Returns: 'Nothing' if the regex did not match the
                -- string, or 'Just' text including before and after text
regexec re tx = do
  mb <- asCStringLen tx $ wrapMatch 0 re
  case mb of
    Right  Nothing     -> return $ Right Nothing
    Right (Just parts) -> return $ Right $ Just $ matchedParts parts
    Left err           -> return $ Left err
  where
    matchedParts [] = (TL.empty,TL.empty,tx,[]) -- no information
    matchedParts (mtchd@(start,stop):rst) =
        ( TL.take (fromIntegral start) tx
        , getSub mtchd
        , TL.drop (fromIntegral stop) tx
        , map getSub rst
        )

    getSub (start,stop)
      | start == unusedOffset = TL.empty
      | otherwise             = TL.take (fromIntegral $ stop-start) $
                                        TL.drop (fromIntegral start) tx


-- ---------------------------------------------------------------------
-- helpers

unwrap :: (Show e) => Either e v -> IO v
unwrap x = case x of
  Left  e -> fail $ "Text.Regex.PCRE.Text died: " ++ show e
  Right v -> return v

{-# INLINE asCString #-}
asCString :: TL.Text -> (CString->IO a) -> IO a
asCString = B.unsafeUseAsCString . T.encodeUtf8 . TL.toStrict

{-# INLINE asCStringLen #-}
asCStringLen :: TL.Text -> (CStringLen->IO a) -> IO a
asCStringLen s op = B.unsafeUseAsCStringLen (T.encodeUtf8 $ TL.toStrict s) checked
  where
    checked cs@(ptr,_)
      | ptr == nullPtr = B.unsafeUseAsCStringLen myEmpty $ op . trim
      | otherwise      = op cs

    trim (ptr,_) = (ptr,0)

myEmpty :: B.ByteString
myEmpty = B.pack [0]
