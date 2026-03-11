
module Biobase.Newick.Export where

import           Data.List (intersperse)
import           Data.Monoid
import           Data.Text.Lazy.Builder (Builder)
import           Data.Text (Text)
import           Data.Tree
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.RealFloat as B

import Biobase.Newick.Types



-- | Render a list of Newick trees to a newick string.

newicksToText :: [NewickTree] -> Text
newicksToText = T.toStrict . B.toLazyText . toTextBuilder

-- | Via builder

toTextBuilder :: [NewickTree] -> Builder
toTextBuilder = mconcat . map ((<> ";\n") . go . getNewickTree)
  where go (Node lbl []) = label lbl
        go (Node lbl cs) = "(" <> mconcat (intersperse "," $ map go cs) <> ")" <> label lbl
        label (Info lbl len) = B.fromText lbl <> (if len == 0 then mempty else ":" <> B.realFloat len)

