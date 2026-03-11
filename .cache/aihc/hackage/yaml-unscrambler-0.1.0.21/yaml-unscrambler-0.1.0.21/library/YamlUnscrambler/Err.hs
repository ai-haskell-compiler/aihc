module YamlUnscrambler.Err
  ( ErrAtPath (..),
    Err (..),
    atSegment,

    -- * --
    MaxInputSize (..),
    Signed (..),
    NumeralSystem (..),
    CaseSensitive (..),

    -- * Reexports from \"libyaml\"
    Libyaml.Tag (..),
    Libyaml.Style (..),
  )
where

import qualified Text.Libyaml as Libyaml
import qualified YamlUnscrambler.Expectations as Ex
import YamlUnscrambler.Model
import YamlUnscrambler.Prelude hiding (String)

data ErrAtPath
  = ErrAtPath [Text] Err

data Err
  = KeyErr
      -- | Key expectation.
      Ex.String
      -- | Key input.
      Text
      -- | String parsing error.
      Text
  | NoneOfMappingKeysFoundErr
      (Ex.ByKey Text)
      CaseSensitive
      -- | Available keys.
      [Text]
      -- | Keys looked up.
      [Text]
  | NoneOfSequenceKeysFoundErr
      (Ex.ByKey Int)
      [Int]
  | ScalarErr
      -- | Expected formats.
      [Ex.Scalar]
      -- | Input.
      ByteString
      -- | Tag.
      Libyaml.Tag
      -- | Style.
      Libyaml.Style
      -- | Last error.
      (Maybe Text)
  | UnexpectedScalarErr
      Ex.Value
  | UnexpectedMappingErr
      Ex.Value
  | UnexpectedSequenceErr
      Ex.Value
  | UnknownAnchorErr
      Text
  | NotEnoughElementsErr
      Ex.ByOrder
      Int

atSegment :: Text -> ErrAtPath -> ErrAtPath
atSegment seg (ErrAtPath path err) =
  ErrAtPath (seg : path) err
