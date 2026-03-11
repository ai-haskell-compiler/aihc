-- |
-- Extra megaparsec combinators.
module HeadedMegaparsec.Megaparsec where

import HeadedMegaparsec.Prelude hiding (head, try)
import Text.Megaparsec hiding (endBy1, sepBy1, sepEndBy1, some, someTill)

contPossibly :: (Ord err, Stream strm) => Parsec err strm (Either a (Parsec err strm a)) -> Parsec err strm a
contPossibly p = do
  junction <- try p
  case junction of
    Left a -> return a
    Right cont -> cont
