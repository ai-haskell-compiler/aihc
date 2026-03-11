module Text.Spintax.RandomPhrase where

import           Data.Either.Combinators (mapRight)
import qualified Data.Text               as T
import           Text.Spintax

newtype RandomPhrase = RandomPhrase { unRandomPhrase :: T.Text }

instance Show RandomPhrase where
  show (RandomPhrase t) = show t

-- | Generate random passphrase or unique id
--
-- >λ> randomPhrase "-" [["blacky","monk","gillespie","coltrane"],["apple","apricot","banana","coconut"],["kant","hegel","husserl","habermas"]]
-- > Right "coltrane-coconut-kant"
--
randomPhrase :: T.Text -> [[T.Text]] -> IO (Either String RandomPhrase)
randomPhrase s ls =
  mapRight RandomPhrase <$> spintax (writeSpintaxExpression s $ writeSpintaxAlternative <$> ls)

