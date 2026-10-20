-- | The export list gives the structure of the documentation. Haddock
-- ignores the section headings in the body.
module ExportBodySections
  ( -- * Values
    answer,
    question,
  )
where

-- * Ignored heading

-- | The answer.
answer :: Int
answer =
  -- | A documentation comment that documents nothing.
  42

-- ** Another ignored heading

-- | The question.
question :: String
question = "unknown"
