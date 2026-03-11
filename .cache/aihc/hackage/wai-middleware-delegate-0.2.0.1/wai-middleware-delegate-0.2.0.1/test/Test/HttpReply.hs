{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.HttpReply
  ( HttpReply (..)
  , HttpReplyMismatch (..)
  , compareHttpReplies
  , assertHttpRepliesAreEq
  , assertHttpRepliesDiffer
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.CaseInsensitive (original)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isJust, isNothing)
import Network.HTTP.Types (Header, HeaderName)


data HttpReply = HttpReply
  { hrSecure :: !Bool
  , hrStatus :: !Int
  , hrHeaders :: ![Header]
  , hrBytes :: !BS.ByteString
  }


instance Show HttpReply where
  show r =
    intercalate "\n" $
      [ "Status: " ++ (show . hrStatus) r
      , "Body:"
      , C8.unpack $ hrBytes r
      ]
        <> (map (show . concatHeader) $ hrHeaders r)
    where
      concatHeader (f, v) = BS.concat ["  ", original f, ": ", v]


data HttpReplyMismatch
  = StatusMismatch Int Int
  | HeaderMismatch
      HeaderName
      (Maybe C8.ByteString)
      (Maybe C8.ByteString)
  | BodyMismatch BS.ByteString BS.ByteString
  | MissingViaHeader
  | UnexpectedViaHeader
  deriving (Eq)


instance Show HttpReplyMismatch where
  show (StatusMismatch x y) = "HTTP status codes don't match : " ++ show x ++ " /= " ++ show y
  show (HeaderMismatch name x y) = "Header field '" ++ show name ++ "' doesn't match : '" ++ show x ++ "' /= '" ++ show y
  show (BodyMismatch x y) = "HTTP response bodies are different :\n" ++ C8.unpack x ++ "\n-----------\n" ++ C8.unpack y
  show MissingViaHeader = "Error: Proxy connection should contain 'X-Via-Proxy' header."
  show UnexpectedViaHeader = "Error: Direct connection should not contain 'X-Via-Proxy' header."


assertHttpRepliesAreEq :: HttpReply -> HttpReply -> IO ()
assertHttpRepliesAreEq direct proxied = do
  let assertNoMismatches [] = return ()
      assertNoMismatches xs = error $ intercalate "\n" $ map show xs
  assertNoMismatches $ compareHttpReplies direct proxied


assertHttpRepliesDiffer :: HttpReply -> HttpReply -> IO ()
assertHttpRepliesDiffer direct proxied = do
  let assertHasMismatches [] = error "Responses should be different!"
      assertHasMismatches _ = return ()
  assertHasMismatches $ compareHttpReplies direct proxied


compareHttpReplies :: HttpReply -> HttpReply -> [HttpReplyMismatch]
compareHttpReplies direct proxied = catMaybes mbMismatches
  where
    mbMismatches =
      [ compare' hrStatus StatusMismatch
      , compareBodys (hrBytes direct) (hrBytes proxied)
      , missingViaHeader
      , unexpectedViaHeader
      , mismatchedHeader "server"
      , mismatchedHeader "content-type"
      , mismatchedHeader "content-length"
      ]
    wantSecure = hrSecure direct
    maybeHeader n r = lookup n $ hrHeaders r
    compare' f g
      | f direct == f proxied = Nothing
      | otherwise = Just $ g (f direct) (f proxied)
    missingViaHeader
      | not wantSecure && (isJust $ maybeHeader "X-Via-Proxy" direct) = Just UnexpectedViaHeader
      | otherwise = Nothing
    unexpectedViaHeader
      | not wantSecure && (isNothing $ maybeHeader "X-Via-Proxy" proxied) = Just MissingViaHeader
      | otherwise = Nothing
    mismatchedHeader name
      | x' /= y' = Just $ HeaderMismatch name x' y'
      | otherwise = Nothing
      where
        x' = maybeHeader name direct
        y' = maybeHeader name proxied


compareBodys :: ByteString -> ByteString -> Maybe HttpReplyMismatch
compareBodys direct proxied =
  let direct' = replaceAmznTraceId direct
      proxied' = replaceAmznTraceId proxied
      compare' a b
        | a == b = Nothing
        | otherwise = Just $ BodyMismatch a b
   in compare' direct' proxied'


replaceAmznTraceId :: ByteString -> ByteString
replaceAmznTraceId x =
  let asLines = C8.lines x
      checkTrace y = "X-Amzn-Trace-Id" `C8.isInfixOf` y
      dropTrace y = if checkTrace y then Nothing else Just y
   in C8.unlines $ catMaybes $ dropTrace <$> asLines
