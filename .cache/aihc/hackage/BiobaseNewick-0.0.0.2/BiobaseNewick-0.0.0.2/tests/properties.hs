
module Main where

import           Debug.Trace
import qualified Data.Aeson as A
import qualified Data.Binary as B
import qualified Data.Serialize as S
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH

import           Biobase.Newick
import qualified Biobase.Newick as N



-- * Serialization to and from the canonical Newick format.

prop_Newick (t :: NewickTree) = Right [t] == ss
  where ss = newicksFromText tt
        tt = newicksToText [t]

-- * Serialization with default Haskell machinery

-- | Correct @Binary@ serialization

prop_Binary (t :: NewickTree) = t == B.decode (B.encode t)

-- | Correct @Cerial@ serialization

prop_Serialize (t :: NewickTree) = Right t == S.decode (S.encode t)

prop_Aeson (t :: NewickTree) = Right t == A.eitherDecode (A.encode t)

main :: IO ()
main = $(defaultMainGenerator)

