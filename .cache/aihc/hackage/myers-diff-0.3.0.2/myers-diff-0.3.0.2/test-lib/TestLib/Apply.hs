
module TestLib.Apply (
  applyChangesText
  , applyChangeText

  , applyChanges
  , applyChange
  ) where

import Data.Diff.Types
import qualified Data.List as L
import Data.Text
import Data.Text.Rope (Rope)
import qualified Data.Text.Rope as Rope


-- * Text

applyChangesText :: [ChangeEvent] -> Text -> Text
applyChangesText changes rope = L.foldl' (flip applyChangeText) rope changes

applyChangeText :: ChangeEvent -> Text -> Text
applyChangeText change t = Rope.toText (applyChange change (Rope.fromText t))

-- * Rope

applyChanges :: [ChangeEvent] -> Rope -> Rope
applyChanges changes rope = L.foldl' (flip applyChange) rope changes

applyChange :: ChangeEvent -> Rope -> Rope
applyChange (ChangeEvent (Range (Position sl sc) (Position fl fc)) txt) str
  = changeChars str (Rope.Position (fromIntegral sl) (fromIntegral sc)) (Rope.Position (fromIntegral fl) (fromIntegral fc)) txt

changeChars :: Rope -> Rope.Position -> Rope.Position -> Text -> Rope
changeChars str start finish new = mconcat [before', Rope.fromText new, after]
 where
   (before, after) = Rope.splitAtPosition finish str
   (before', _) = Rope.splitAtPosition start before
