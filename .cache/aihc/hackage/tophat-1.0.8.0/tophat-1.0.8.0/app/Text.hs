{-# LANGUAGE
    BangPatterns
  #-}

module Text where

{-
  Helper functions for text processing

  Part of this is just patching the Text.Lazy module with some
  slightly more general functions.
-}


import Prelude (Char, Int, Maybe(..), (+), (.), ($), (==), (++))
import qualified Prelude as P
import Data.String (IsString, fromString)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as TS
import Data.Text.Internal.Lazy (Text(..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Unsafe as TS


-- | How many line breaks are there in this text?
linebreaks :: Text -> Int
linebreaks = TL.foldl' f 0 where
  f n '\n' = n+1
  f n _ = n


-- | Like breakOn, but indicates clearly whether there was a match or
-- not. It's surprising this isn't in the standard library.
breakOnOnce :: Text -> Text -> Maybe (Text, Text)
breakOnOnce pat src = let (a, b) = TL.breakOn pat src in
  if TL.null b then Nothing else Just (a, b)


-- | As above, but omits the match text. It's even more surprising
-- that this isn't in the standard library.
splitOnOnce :: Text -> Text -> Maybe (Text, Text)
splitOnOnce pat src = case breakOnOnce pat src of
  Nothing     -> Nothing
  Just (a, b) -> Just (a, TL.drop (TL.length pat) b)


initsNE :: Text -> NonEmpty Text
initsNE = (Empty :|) . inits'
  where inits' Empty        = []
        inits' (Chunk t ts) = P.map (\t' -> Chunk t' Empty) (P.drop 1 (TS.inits t))
                           ++ P.map (Chunk t) (inits' ts)

-- | A version of `Text.tails` that returns a NonEmpty
tailsNE :: Text -> NonEmpty Text
tailsNE Empty         = Empty :| []
tailsNE ts@(Chunk t ts')
  | TS.length t == 1 = ts :| TL.tails ts'
  | P.otherwise      = ts :| TL.tails (Chunk (TS.unsafeTail t) ts')

-- | The characters of t, together with the complete text of t up to
-- that point, and the complete text thereafter. So, for all (c,(u,v))
-- in splits t, we have t = u <> c <> v.
splits :: Text -> [(Char, (Text, Text))]
splits t = P.zip (TL.unpack t) $ P.zip (TL.inits t) (NE.tail $ tailsNE t)


-- | As above, but this time the former includes the character just
-- gone. So, for all (c,(u,v)) in splits' t, we have t = u <> v.
splits' :: Text -> [(Char, (Text, Text))]
splits' t = P.zip (TL.unpack t) . NE.tail $ NE.zip (initsNE t) (tailsNE t)


-- | Formats text as a literal string
textLiteral :: (IsString s) => Text -> s
textLiteral = fromString . P.show . TL.unpack
