module Web.HttpApiData.QQ.Parser (
  ParsedUrlPiece (..),
  parseUrlPieces,
) where

import Control.Monad (unless)
import Text.ParserCombinators.ReadP hiding (choice, many1)

data ParsedUrlPiece
  = RawString String
  | InterpolatedName String
  deriving (Show, Eq)

parseUrlPieces :: String -> Either String [ParsedUrlPiece]
parseUrlPieces = runParser (manyTill parseUrlPiece eof)

parseUrlPiece :: ReadP ParsedUrlPiece
parseUrlPiece =
  choice
    [ InterpolatedName <$> between (string "#{") (string "}") (many1 $ anySingleBut '}')
    , RawString <$> many1 (notFollowedBy (string "#{") >> anySingle)
    ]

{--
Parser utilities

Using ReadP since it's in base, to avoid pulling down extra dependencies,
but defining helpers here to roughly mimic megaparsec's API, which is more
readable than ReadP's API.
--}

runParser :: (Show a) => ReadP a -> String -> Either String a
runParser p s =
  case filter (null . snd) (readP_to_S p s) of
    [(x, "")] -> Right x
    [] -> Left "Could not parse input"
    result -> Left $ "Ambiguous parse: " ++ show result

-- | Same as ReadP's 'choice', except using (<++) instead of (+++)
choice :: [ReadP a] -> ReadP a
choice = foldr (<++) pfail

-- | See megaparsec's 'anySingle'.
anySingle :: ReadP Char
anySingle = get

-- | See megaparsec's 'anySingleBut'.
anySingleBut :: Char -> ReadP Char
anySingleBut c = satisfy (/= c)

-- | Same as ReadP's 'many1', except using (<++) instead of (+++)
many1 :: ReadP a -> ReadP [a]
many1 p = (:) <$> p <*> (many1 p <++ return [])

-- | See megaparsec's 'notFollowedBy'.
notFollowedBy :: ReadP a -> ReadP ()
notFollowedBy p = do
  failed <- (p >> pure False) <++ pure True
  unless failed pfail
