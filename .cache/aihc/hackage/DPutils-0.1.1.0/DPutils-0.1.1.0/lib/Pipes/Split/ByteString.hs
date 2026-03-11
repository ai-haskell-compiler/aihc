
-- | Split incombing bytestrings based on bytestrings.

module Pipes.Split.ByteString where

import           Control.Monad (join,unless)
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString (ByteString)
import           Data.ByteString.Search (indices)
import           Data.Monoid ((<>))
import           Debug.Trace
import           Pipes (Producer,next,yield)
import qualified Data.ByteString as BS



type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

-- | Splits bytestrings after each pattern @pat@. Tries to minimize the
-- number of intermediate bytestring constructors.
--
-- The following function @ske@ expects a string @str@ and a pattern @pat@
-- and then returns a tuple with the splitted bytestrings in @fst@ and the
-- return value in @snd@.
--
-- The inner parser @parse@ uses @zoom@ to draw the full inner producer,
-- which should contain just one bytestring, namely one of the split off
-- ones. @parse@ doesn't do anything with the inner producer, except
-- returning the contained bytestring.
--
-- @parse@ returns @Right $ concat xs@ on a correct parse, and @Left []@
-- once the input has been exhausted.
--
-- @
-- ske :: ByteString -> ByteString -> ([ByteString],[ByteString],[ByteString])
-- ske pat str | BS.null pat || BS.null str = ([],[],[])
-- ske pat str =
--   let parse = do
--         xs <- zoom (splitKeepEnd pat) PP.drawAll
--         case xs of
--           [] -> return $ Left []
--           xs -> return $ Right $ BS.concat xs
--       (a,(b,p)) = runIdentity . P.toListM' $ PP.parsed parse $ PP.yield str
--   in (a,b, fst . runIdentity . P.toListM' $ p)
-- @

splitKeepEnd :: Monad m => ByteString -> Lens' (Producer ByteString m x) (Producer ByteString m (Producer ByteString m x))
splitKeepEnd pat k p0 = fmap join (k (go BS.empty p0)) where
  go pre p = do
    x <- lift (next p)
    case x of
      Left r -> return $ return r
      Right (bs, p') -> do
        case fnd (pre <> bs) of
          -- no hit yet, send the bs down, keep some suffix
          [] -> do
            unless (BS.null bs) (yield bs)
            let pfx = BS.drop (BS.length bs - l + 1) bs
            go pfx p'
          -- at least one hit, split off the correct part, remainder goes
          -- back.
          (k:_) -> do
            let (y,suf) = BS.splitAt (k - BS.length pre + l) bs
            yield y
            return (yield suf >> p')
  l = BS.length pat
  fnd = indices pat
{-# Inlineable splitKeepEnd #-}



-- | Split a string into substrings, where each substring starts with @pat@
-- and continues until just before the next @pat@ (or until there is no
-- more input).
--
-- Any prefix that does not start with the substring is /kept/!
--
-- Since each substring is supposed to start with @pat@, there is a small
-- problem. What about a header that prefixes the string we are interested
-- in?

splitKeepStart :: Monad m => ByteString -> Lens' (Producer ByteString m x) (Producer ByteString m (Producer ByteString m x))
splitKeepStart = splitGeneric (\bs k p l -> BS.splitAt (k - p) bs)
{-# Inlineable splitKeepStart #-}



-- | Generic splitting function. Takes a bytestring @[a,b,c]@ (where
-- @a,b,c@ are substrings of the bytestring!) and performs the split.
--

splitGeneric
  :: Monad m
  => (ByteString -> Int -> Int -> Int -> (ByteString,ByteString))
  -- ^ splitter function
  -> ByteString
  -- ^ pattern to split on
  -> Lens' (Producer ByteString m x) (Producer ByteString m (Producer ByteString m x))
  -- ^ lens into the individual split off bytestrings
splitGeneric splt pat k p0 = fmap join (k (go BS.empty p0)) where
  go pre p = do
    x <- lift (next p)
    case x of
      Left r -> do
        -- yield final split off string
        unless (BS.null pre) (yield pre)
        return $ return r
      Right (bs, p') -> do
        -- will not search in the part of the prefix that *can not contain*
        -- the @pat@tern.
        case fnd ((BS.drop (BS.length pre - l) pre) <> bs) of
          -- no hit yet, send the prefix down completely, make bs new
          -- prefix if possible. If either @pre@ or @bs@ are too short, we
          -- keep @pre <> bs@ for the next round. This should not happen if
          -- the pattern is reasonably short compared to the size of the
          -- bytestring chunks.
          [] -> do
            if (BS.length bs >= l)
            then yield pre >> go bs p'
            else go (pre <> bs) p'
          -- at least one hit, split off the correct part, remainder goes
          -- back.
          (k:_) -> do
            let (y,suf) = splt bs k (BS.length pre) l
            yield y
            return (yield suf >> p')
  l = BS.length pat
  fnd = indices pat
{-# Inline splitGeneric #-}



-- manual splitting, for @splitKeepEnd@

referenceByteStringTokenizer pat str | BS.null pat || BS.null str = []
referenceByteStringTokenizer pat str
  = (h `BS.append` BS.take (BS.length pat) t)
  : if BS.null t then [] else referenceByteStringTokenizer pat (BS.drop (BS.length pat) t)
    where (h,t) = BS.breakSubstring pat str

