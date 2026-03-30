{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cpp.Scanner
  ( LineSpan (..),
    LineScan (..),
    scanLine,
    expandLineBySpan,
  )
where

import Aihc.Cpp.Evaluator (expandMacros)
import Aihc.Cpp.Types (EngineState)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB

data LineSpan = LineSpan
  { lineSpanInBlockComment :: !Bool,
    lineSpanText :: !Text
  }

data LineScan = LineScan
  { lineScanSpans :: ![LineSpan],
    lineScanFinalHsDepth :: !Int,
    lineScanFinalCDepth :: !Int
  }

expandLineBySpan :: EngineState -> [LineSpan] -> Text
expandLineBySpan st =
  T.concat . map expandSpan
  where
    expandSpan lineChunk
      | lineSpanInBlockComment lineChunk = lineSpanText lineChunk
      | otherwise = expandMacros st (lineSpanText lineChunk)

scanLine :: Int -> Int -> Text -> LineScan
scanLine hsDepth0 cDepth0 input =
  let (spansRev, currentBuilder, hasCurrent, currentInComment, finalHsDepth, finalCDepth) =
        go hsDepth0 cDepth0 False False False [] mempty False (hsDepth0 > 0 || cDepth0 > 0) input
      spans = reverse (flushSpan spansRev currentBuilder hasCurrent currentInComment)
   in LineScan
        { lineScanSpans = spans,
          lineScanFinalHsDepth = finalHsDepth,
          lineScanFinalCDepth = finalCDepth
        }
  where
    flushSpan :: [LineSpan] -> TB.Builder -> Bool -> Bool -> [LineSpan]
    flushSpan spansRev currentBuilder hasCurrent inComment =
      if not hasCurrent
        then spansRev
        else LineSpan {lineSpanInBlockComment = inComment, lineSpanText = builderToText currentBuilder} : spansRev

    appendWithMode ::
      [LineSpan] ->
      TB.Builder ->
      Bool ->
      Bool ->
      Bool ->
      Text ->
      ([LineSpan], TB.Builder, Bool, Bool)
    appendWithMode spansRev currentBuilder hasCurrent currentInComment newInComment chunk
      | T.null chunk = (spansRev, currentBuilder, hasCurrent, currentInComment)
      | not hasCurrent = (spansRev, TB.fromText chunk, True, newInComment)
      | currentInComment == newInComment = (spansRev, currentBuilder <> TB.fromText chunk, True, currentInComment)
      | otherwise =
          let spansRev' = flushSpan spansRev currentBuilder hasCurrent currentInComment
           in (spansRev', TB.fromText chunk, True, newInComment)

    go ::
      Int ->
      Int ->
      Bool ->
      Bool ->
      Bool ->
      [LineSpan] ->
      TB.Builder ->
      Bool ->
      Bool ->
      Text ->
      ([LineSpan], TB.Builder, Bool, Bool, Int, Int)
    go hsDepth cDepth inString inChar escaped spansRev currentBuilder hasCurrent currentInComment remaining =
      case T.uncons remaining of
        Nothing -> (spansRev, currentBuilder, hasCurrent, currentInComment, hsDepth, cDepth)
        Just (c1, rest1) ->
          case T.uncons rest1 of
            Nothing ->
              let outChar = if cDepth > 0 then " " else T.singleton c1
                  inCommentNow = hsDepth > 0 || cDepth > 0
                  (spansRev', currentBuilder', hasCurrent', currentInComment') =
                    appendWithMode spansRev currentBuilder hasCurrent currentInComment inCommentNow outChar
               in (spansRev', currentBuilder', hasCurrent', currentInComment', hsDepth, cDepth)
            Just (c2, rest2) ->
              if cDepth > 0
                then
                  if c1 == '*' && c2 == '/'
                    then
                      let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                            appendWithMode spansRev currentBuilder hasCurrent currentInComment True "  "
                       in go hsDepth 0 False False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                    else
                      let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                            appendWithMode spansRev currentBuilder hasCurrent currentInComment True " "
                       in go hsDepth cDepth False False False spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                else
                  if not inString && not inChar && hsDepth == 0 && c1 == '-' && c2 == '-'
                    then
                      let lineTail = T.cons c1 (T.cons c2 rest2)
                          (spansRev', currentBuilder', hasCurrent', currentInComment') =
                            appendWithMode spansRev currentBuilder hasCurrent currentInComment True lineTail
                       in (spansRev', currentBuilder', hasCurrent', currentInComment', hsDepth, cDepth)
                    else
                      if inString
                        then
                          let escaped' = not escaped && c1 == '\\'
                              inString' = escaped || c1 /= '"'
                              (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                appendWithMode spansRev currentBuilder hasCurrent currentInComment (hsDepth > 0) (T.singleton c1)
                           in go hsDepth cDepth inString' False escaped' spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                        else
                          if inChar
                            then
                              let escaped' = not escaped && c1 == '\\'
                                  inChar' = escaped || c1 /= '\''
                                  (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                    appendWithMode spansRev currentBuilder hasCurrent currentInComment (hsDepth > 0) (T.singleton c1)
                               in go hsDepth cDepth False inChar' escaped' spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                            else
                              if hsDepth == 0 && c1 == '"'
                                then
                                  let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                        appendWithMode spansRev currentBuilder hasCurrent currentInComment False (T.singleton c1)
                                   in go hsDepth cDepth True False False spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                                else
                                  if hsDepth == 0 && c1 == '\''
                                    then
                                      let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                            appendWithMode spansRev currentBuilder hasCurrent currentInComment False (T.singleton c1)
                                       in go hsDepth cDepth False True False spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)
                                    else
                                      if hsDepth > 0 && c1 == '-' && c2 == '}'
                                        then
                                          let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                                appendWithMode spansRev currentBuilder hasCurrent currentInComment True "-}"
                                              hsDepth' = hsDepth - 1
                                           in go hsDepth' cDepth False False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                                        else
                                          if c1 == '{' && c2 == '-' && not ("#" `T.isPrefixOf` rest2)
                                            then
                                              let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                                    appendWithMode spansRev currentBuilder hasCurrent currentInComment True "{-"
                                               in go (hsDepth + 1) cDepth False False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                                            else
                                              if hsDepth == 0 && c1 == '/' && c2 == '*'
                                                then
                                                  let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                                        appendWithMode spansRev currentBuilder hasCurrent currentInComment True "  "
                                                   in go hsDepth 1 False False False spansRev' currentBuilder' hasCurrent' currentInComment' rest2
                                                else
                                                  let (spansRev', currentBuilder', hasCurrent', currentInComment') =
                                                        appendWithMode spansRev currentBuilder hasCurrent currentInComment (hsDepth > 0) (T.singleton c1)
                                                   in go hsDepth cDepth False False False spansRev' currentBuilder' hasCurrent' currentInComment' (T.cons c2 rest2)

builderToText :: TB.Builder -> Text
builderToText = TL.toStrict . TB.toLazyText
