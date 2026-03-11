import           Data.Monoid

import qualified Data.Vector          as V
import           Data.Vector.Distance

-- | Editing vectors of 'Char' values, with '(String, Int, Char)' describing
--   changes, and the additive monoid of 'Int' describing costs.
str :: Params Char (String, Int, Char) (Sum Int)
str = Params
    { equivalent = (==)
    , delete     = \i c    -> ("delete", i, c)
    , insert     = \i c    -> ("insert", i, c)
    , substitute = \i c c' -> ("replace", i, c')
    , cost = const (Sum 1)
    , positionOffset = \ (op, _, _) -> if op == "delete" then 0 else 1
    }

main :: IO ()
main = do
    print $ leastChanges str (V.fromList "I am thomas")
                             (V.fromList "My name is Thomas")
