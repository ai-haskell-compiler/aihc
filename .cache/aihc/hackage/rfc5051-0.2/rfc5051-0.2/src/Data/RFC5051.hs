module Data.RFC5051 (compareUnicode)
where
import Data.RFC5051.UnicodeData (decompositionMap)
import qualified Data.Map as M
import Data.Char (ord, toTitle)
import Data.Text (Text)
import qualified Data.Text as T

-- | Compare two strings using @i;unicode-casemap@,
-- the simple unicode collation algorithm described in RFC 5051.
compareUnicode :: Text -> Text -> Ordering
compareUnicode x y =
  case (T.uncons x, T.uncons y) of
    (Nothing, Nothing) -> EQ
    (Nothing, Just _)  -> LT
    (Just _, Nothing)  -> GT
    (Just (xc,x'), Just (yc,y')) ->
      case compare (canonicalize xc) (canonicalize yc) of
        GT -> GT
        LT -> LT
        EQ -> compareUnicode x' y'

canonicalize :: Char -> [Int]
canonicalize = decompose . ord . toTitle

decompose :: Int -> [Int]
decompose c =
  case decompose' c of
         Nothing -> [c]
         Just xs -> concatMap decompose xs

decompose' :: Int -> Maybe [Int]
decompose' c = M.lookup c decompositionMap
