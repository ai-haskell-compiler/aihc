{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cpp.Scanner
  ( LineSpan (..),
    LineScan (..),
    scanLine,
    scanLineDepthOnly,
    expandLineBySpanMultiline,
  )
where

import Aihc.Cpp.Cursor
  ( Cursor (..),
    advance,
    advance2,
    bufLength,
    findNewline,
    null,
    peekByte,
    peekByte2,
    skipNewline,
    skipToInteresting,
    sliceText,
  )
import Aihc.Cpp.Evaluator (expandMacros, expandMacrosMultiline)
import Aihc.Cpp.Types (EngineState)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (null)

data LineSpan = LineSpan
  { lineSpanInBlockComment :: !Bool,
    lineSpanText :: !Text
  }

data LineScan = LineScan
  { lineScanSpans :: ![LineSpan],
    lineScanFinalHsDepth :: !Int,
    lineScanFinalCDepth :: !Int
  }

-- | Expand macros in a list of line spans (single-line, no lookahead).
expandLineBySpan :: EngineState -> [LineSpan] -> Text
expandLineBySpan st =
  T.concat . map expandSpan
  where
    expandSpan lineChunk
      | lineSpanInBlockComment lineChunk = lineSpanText lineChunk
      | otherwise = expandMacros st (lineSpanText lineChunk)

-- | Expand macros in a list of line spans with multi-line lookahead.
-- When a function macro call spans multiple lines, continuation lines
-- are consumed from the @futureCursor@ (positioned after the current line).
-- Returns (expanded text, number of extra lines consumed).
--
-- Multi-line expansion is only attempted for lines that consist entirely
-- of code spans (no inline comments). Mixed code/comment lines use
-- single-line expansion to preserve comment span positions.
expandLineBySpanMultiline :: EngineState -> [LineSpan] -> Cursor -> (Text, Int)
expandLineBySpanMultiline st spans futureCursor =
  let commentSpans = filter lineSpanInBlockComment spans
      hasLineComment = any (\s -> "--" `T.isPrefixOf` lineSpanText s) commentSpans
      hasBlockComment = any (\s -> not ("--" `T.isPrefixOf` lineSpanText s)) commentSpans
   in if hasLineComment && not hasBlockComment
        then -- Line with -- comment: full-line expansion so macro args can span into the comment
          let fullText = T.concat [lineSpanText s | s <- spans]
           in (expandMacros st fullText, 0)
        else if hasBlockComment
          then -- Block comment spans: fall back to per-span expansion
            (expandLineBySpan st spans, 0)
          else -- Pure code line: try multi-line expansion
            let codeText = T.concat [lineSpanText s | s <- spans]
                futureCodeLines = cursorToLines futureCursor
             in expandMacrosMultiline st codeText futureCodeLines

-- | Extract lines from a cursor as a lazy list of Text values.
-- Each line is the text up to the next newline (or EOF).
cursorToLines :: Cursor -> [Text]
cursorToLines !cur
  | null cur = []
  | otherwise =
      let eol = findNewline cur
          lineText = sliceText (curPos cur) (curPos eol) cur
       in lineText : maybe [] cursorToLines (skipNewline eol)

-- | Lightweight scan that only tracks block comment depth changes.
-- Does not build 'LineSpan' segments or track string/char literals.
-- Used for inactive conditional branches where only comment depth
-- tracking is needed (no macro expansion or span splitting).
--
-- Accepts a 'Cursor' positioned at the start of the line content.
-- The cursor should be bounded to the line (e.g., via 'lineSlice').
scanLineDepthOnly :: Int -> Int -> Cursor -> (Int, Int)
scanLineDepthOnly = goDepth
  where
    goDepth :: Int -> Int -> Cursor -> (Int, Int)
    goDepth !hsDepth !cDepth !cur
      | null cur = (hsDepth, cDepth)
      | otherwise =
          case peekByte2 cur of
            Nothing ->
              -- One byte left, no two-char sequence possible
              (hsDepth, cDepth)
            Just (b1, b2)
              | cDepth > 0 ->
                  if b1 == 0x2A && b2 == 0x2F -- '*/'
                    then goDepth hsDepth 0 (advance2 cur)
                    else goDepth hsDepth cDepth (advance cur)
              | hsDepth > 0 && b1 == 0x2D && b2 == 0x7D -> -- '-}'
                  goDepth (hsDepth - 1) cDepth (advance2 cur)
              | b1 == 0x7B && b2 == 0x2D -> -- '{-'
                  let cur' = advance2 cur
                   in case peekByte cur' of
                        Just 0x23 ->
                          -- {-# is a pragma, not a comment
                          goDepth hsDepth cDepth (advance cur)
                        _ ->
                          goDepth (hsDepth + 1) cDepth cur'
              | hsDepth == 0 && b1 == 0x2F && b2 == 0x2A -> -- '/*'
                  goDepth hsDepth 1 (advance2 cur)
              | hsDepth == 0 && b1 == 0x2D && b2 == 0x2D -> -- '--' line comment
                  (hsDepth, cDepth)
              | otherwise ->
                  goDepth hsDepth cDepth (advance cur)

-- | Scan a line, tracking comment depths and splitting into spans that are
-- either inside or outside block comments. Uses a byte-level cursor for
-- efficient scanning instead of character-by-character T.uncons/T.cons.
--
-- Accepts a 'Cursor' positioned at the start of the line content.
-- The cursor should be bounded to the line (e.g., via 'lineSlice').
--
-- The scanner splits the line into 'LineSpan' segments. Each segment is
-- tagged with whether it is inside a block comment. Code spans (outside
-- comments) are zero-copy slices of the UTF-8 encoded input. C89 comment
-- content is replaced with spaces to preserve column alignment.
scanLine :: Int -> Int -> Cursor -> LineScan
scanLine hsDepth0 cDepth0 cursor0 =
  let (spans, finalHsDepth, finalCDepth) =
        go
          hsDepth0
          cDepth0
          False
          False
          False
          []
          (curPos cursor0)
          (hsDepth0 > 0 || cDepth0 > 0)
          cursor0
   in LineScan
        { lineScanSpans = reverse spans,
          lineScanFinalHsDepth = finalHsDepth,
          lineScanFinalCDepth = finalCDepth
        }
  where
    -- \| Emit a span from @start@ to @end@ if non-empty, prepending to @acc@.
    emit :: [LineSpan] -> Int -> Int -> Cursor -> Bool -> [LineSpan]
    emit acc start end cur inComment
      | start >= end = acc
      | otherwise = LineSpan inComment (sliceText start end cur) : acc
    {-# INLINE emit #-}

    go ::
      Int ->
      Int ->
      Bool ->
      Bool ->
      Bool ->
      [LineSpan] ->
      Int ->
      Bool ->
      Cursor ->
      ([LineSpan], Int, Int)
    go
      !hsDepth
      !cDepth
      !inString
      !inChar
      !escaped
      !acc
      !spanStart
      !spanInComment
      !cur
        -- End of input: flush the accumulated span
        | null cur =
            (emit acc spanStart (curPos cur) cur spanInComment, hsDepth, cDepth)
        | otherwise =
            case peekByte2 cur of
              Nothing ->
                -- === Only one byte left ===
                if cDepth > 0
                  then
                    -- In C comment: flush accumulated, emit space
                    let acc' = emit acc spanStart (curPos cur) cur spanInComment
                     in (LineSpan True " " : acc', hsDepth, cDepth)
                  else
                    -- Include this last byte in the accumulated span
                    let inCommentNow = hsDepth > 0
                        cur' = advance cur
                     in (emit acc spanStart (curPos cur') cur inCommentNow, hsDepth, cDepth)
              Just (b1, b2) ->
                -- === C block comment mode ===
                if cDepth > 0
                  then
                    if b1 == 0x2A && b2 == 0x2F -- '*/'
                      then
                        let acc' = emit acc spanStart (curPos cur) cur spanInComment
                            cur' = advance2 cur
                         in go
                              hsDepth
                              0
                              False
                              False
                              False
                              (LineSpan True "  " : acc')
                              (curPos cur')
                              False
                              cur'
                      else
                        let acc' = emit acc spanStart (curPos cur) cur spanInComment
                            cur' = advance cur
                         in go
                              hsDepth
                              cDepth
                              False
                              False
                              False
                              (LineSpan True " " : acc')
                              (curPos cur')
                              True
                              cur'
                  -- === Line comment: -- (outside strings and hs comments) ===
                  else
                    if not inString
                      && not inChar
                      && hsDepth == 0
                      && b1 == 0x2D
                      && b2 == 0x2D -- '--'
                      then
                        let acc' = emit acc spanStart (curPos cur) cur spanInComment
                            restText = sliceText (curPos cur) (bufLength cur) cur
                         in (LineSpan True restText : acc', hsDepth, cDepth)
                      -- === Inside string literal ===
                      else
                        if inString
                          then
                            let escaped' = not escaped && b1 == 0x5C -- '\\'
                                inString' = escaped || b1 /= 0x22 -- '"'
                             in go
                                  hsDepth
                                  cDepth
                                  inString'
                                  False
                                  escaped'
                                  acc
                                  spanStart
                                  spanInComment
                                  (advance cur)
                          -- === Inside char literal ===
                          else
                            if inChar
                              then
                                let escaped' = not escaped && b1 == 0x5C -- '\\'
                                    inChar' = escaped || b1 /= 0x27 -- '\''
                                 in go
                                      hsDepth
                                      cDepth
                                      False
                                      inChar'
                                      escaped'
                                      acc
                                      spanStart
                                      spanInComment
                                      (advance cur)
                              -- === Start of string literal ===
                              else
                                if hsDepth == 0 && b1 == 0x22 -- '"'
                                  then
                                    go
                                      hsDepth
                                      cDepth
                                      True
                                      False
                                      False
                                      acc
                                      spanStart
                                      spanInComment
                                      (advance cur)
                                  -- === Start of char literal ===
                                  else
                                    if hsDepth == 0 && b1 == 0x27 -- '\''
                                      then
                                        go
                                          hsDepth
                                          cDepth
                                          False
                                          True
                                          False
                                          acc
                                          spanStart
                                          spanInComment
                                          (advance cur)
                                      -- === End of Haskell block comment: -} ===
                                      else
                                        if hsDepth > 0 && b1 == 0x2D && b2 == 0x7D -- '-}'
                                          then
                                            let cur' = advance2 cur
                                                hsDepth' = hsDepth - 1
                                                -- Flush everything up to and including -} as a comment span
                                                acc' = emit acc spanStart (curPos cur') cur True
                                                inCommentAfter = hsDepth' > 0
                                             in go
                                                  hsDepth'
                                                  cDepth
                                                  False
                                                  False
                                                  False
                                                  acc'
                                                  (curPos cur')
                                                  inCommentAfter
                                                  cur'
                                          -- === Start of Haskell block comment: {- (but not {-#) ===
                                          else
                                            if b1 == 0x7B && b2 == 0x2D -- '{-'
                                              then
                                                let cur' = advance2 cur
                                                 in case peekByte cur' of
                                                      Just 0x23 ->
                                                        -- '#' => pragma {-#, not a block comment
                                                        -- Advance past '{' only, continue in same mode
                                                        go
                                                          hsDepth
                                                          cDepth
                                                          False
                                                          False
                                                          False
                                                          acc
                                                          spanStart
                                                          spanInComment
                                                          (advance cur)
                                                      _ ->
                                                        -- Flush any text before {-, emit {- as comment
                                                        let acc' = emit acc spanStart (curPos cur) cur spanInComment
                                                            acc'' = LineSpan True "{-" : acc'
                                                         in go
                                                              (hsDepth + 1)
                                                              cDepth
                                                              False
                                                              False
                                                              False
                                                              acc''
                                                              (curPos cur')
                                                              True
                                                              cur'
                                              -- === Start of C block comment: /* ===
                                              else
                                                if hsDepth == 0 && b1 == 0x2F && b2 == 0x2A -- '/*'
                                                  then
                                                    let acc' = emit acc spanStart (curPos cur) cur spanInComment
                                                        cur' = advance2 cur
                                                     in go
                                                          hsDepth
                                                          1
                                                          False
                                                          False
                                                          False
                                                          (LineSpan True "  " : acc')
                                                          (curPos cur')
                                                          True
                                                          cur'
                                                  -- === Normal byte: bulk-skip non-interesting bytes ===
                                                  else
                                                    let cur' = skipToInteresting (advance cur)
                                                     in go
                                                          hsDepth
                                                          cDepth
                                                          False
                                                          False
                                                          False
                                                          acc
                                                          spanStart
                                                          spanInComment
                                                          cur'
