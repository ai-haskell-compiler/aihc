{-# LANGUAGE OverloadedStrings #-}
module Data.Prometheus.Parse
  ( parseMetrics
  ) where

import Control.Applicative

import Prelude hiding (takeWhile)
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map

import Data.Prometheus.Types

parseMetrics :: Parser (Map MetricId Metric)
parseMetrics = Data.Map.fromList . concat <$>
  many1 parseMetric <* many parseError <* endOfInput

parseMetric :: Parser [(MetricId, Metric)]
parseMetric = do
  (name, help, typ) <- parseMeta
  lm <- case typ of
    "counter" -> parseCounters
    "gauge" -> parseGauges
    "untyped" -> parseGauges -- untyped /o\
    "histogram" -> parseHistogram
    "summary" -> parseSummary
    x -> fail $ show x

  pure $ map (\(labels, metric) -> (MetricId name help labels, metric)) lm

-- name, help, textual type
parseMeta :: Parser (Text, Text, Text)
parseMeta = do
  _ <- "# HELP "
  name <- word
  _ <- space
  help <- eol
  endOfLine
  _ <- "# TYPE "
  _ <- word -- repeated name
  _ <- space
  typ <- word
  endOfLine
  pure (name, help, typ)
  where
    eol :: Parser Text
    eol = takeWhile (/= '\n')

    word :: Parser Text
    word  = takeWhile1 (\x -> x /=' ' && x /= '\n')

parseGauges :: Parser [(Map Text Text, Metric)]
parseGauges = many1 (labelsValue (Gauge <$> double))

parseCounters :: Parser [(Map Text Text, Metric)]
parseCounters = many1 (labelsValue (Counter <$> double))

parseSummary :: Parser [(Map Text Text, Metric)]
parseSummary = do
  qs <- Data.Map.fromList <$> parseQuantiles `sepBy` endOfLine <?> "quantiles"
  (_, lsum) <- labelsValue double
  (_, lcnt) <- labelsValue double
  pure $ [(mempty, Summary qs lsum lcnt)]

parseQuantiles :: Parser (Double, Double)
parseQuantiles = do
  _ <- takeWhile1 (\x -> x /= '{' && x /= ' ')
  q <- "{quantile=\"" *> double <* "\"}"
  _ <- space
  val <- double
  pure (q, val)

parseHistogram :: Parser [(Map Text Text, Metric)]
parseHistogram = do
  qs <- Data.Map.fromList <$> parseHistBuckets `sepBy` endOfLine <?> "quantiles"
  (_, lsum) <- labelsValue double
  (_, lcnt) <- labelsValue double
  pure $ [(mempty, Histogram qs lsum lcnt)]

parseHistBuckets :: Parser (Double, Double)
parseHistBuckets = do
  _ <- takeWhile1 (\x -> x /= '{' && x /= ' ')
  q <- "{le=\"" *> double <* "\"}"
  _ <- space
  val <- double
  pure (q, val)

labelsValue :: Parser b -> Parser (Map Text Text, b)
labelsValue f = do
  _ <- takeWhile1 (\x -> x /= '{' && x /= ' ')
  ls <- option mempty (char '{' *> parseLabels <* char '}')
  _ <- space
  val <- f
  endOfLine
  pure (ls, val)

parseLabels :: Parser (Map Text Text)
parseLabels = Data.Map.fromList <$> parseLabel `sepBy1` (char ',')

parseLabel :: Parser (Text, Text)
parseLabel = do
  l <- takeWhile (/= '=')
  _ <- char '='
  v <- char '"' *> takeWhile (\x -> x /= '"') <* char '"'
  pure (l, v)

parseError :: Parser Text
parseError = do
  _ <- "# ERROR "
  err <- eol
  endOfLine
  pure err
  where
    eol :: Parser Text
    eol = takeWhile (/= '\n')
