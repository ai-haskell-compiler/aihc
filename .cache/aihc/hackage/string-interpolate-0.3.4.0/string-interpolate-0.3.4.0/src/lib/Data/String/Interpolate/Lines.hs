{-# LANGUAGE LambdaCase #-}

module Data.String.Interpolate.Lines where

import Data.Function  ( on )
import Data.List      ( find )
import Data.Semigroup ( Min(..) )

import Data.String.Interpolate.Types

isBlankLine :: [InterpSegment] -> Bool
isBlankLine [] = True
isBlankLine (Expression _ : _) = False
isBlankLine (Spaces _ : rest) = isBlankLine rest
isBlankLine (Tabs _ : rest) = isBlankLine rest
isBlankLine (Verbatim str:rest) = blank str && isBlankLine rest
  where
    blank :: String -> Bool
    blank = all (\c -> elem c [' ', '\t'])

-- |
-- Go the other direction, from a `Line' to the input that produced it.
displayLine :: Line -> String
displayLine = foldMap displaySegment
  where
    displaySegment :: InterpSegment -> String
    displaySegment (Expression expr) = "#{" ++ expr ++ "}"
    displaySegment (Verbatim str)    = str
      -- Above case is technically not correct due to escaped characters,
      -- but for the purposes of pinpointing where in a user's interpolation
      -- a problem is, it's good enough.
    displaySegment (Spaces n)        = replicate n ' '
    displaySegment (Tabs n)          = replicate n '\t'

-- |
-- Remove min indentation from given lines, using the minimum indentation
-- found within the lines. Gives back warnings for mixed indentation if
-- any are found.
handleIndents :: Lines -> ([IndentWarning], Lines)
handleIndents lines =
  let mindent = mindentation lines
  in (findMixedIndents mindent lines, reduceIndents mindent lines)

data Mindent = UsesSpaces Int | UsesTabs Int

data IndentWarning = IndentWarning
  { indentLine :: String
  , indentBase :: Mindent
  }

mindentation :: Lines -> Mindent
mindentation lines =
  let
    nonblank = filter (not . isBlankLine) lines
    withIndent = find (\case { Spaces _ : _ -> True; Tabs _ : _ -> True; _ -> False }) nonblank
  in case withIndent of
      Nothing -> UsesSpaces 0
      Just (Spaces _ : _) ->
        maybe (UsesSpaces 0) UsesSpaces $
          findMinIndent (\case { Spaces n -> Just n; _ -> Nothing }) Nothing nonblank
      Just (Tabs _ : _) ->
        maybe (UsesSpaces 0) UsesTabs $
          findMinIndent (\case { Tabs n -> Just n; _ -> Nothing }) Nothing nonblank
      Just _ -> UsesSpaces 0
  where
    findMinIndent :: (InterpSegment -> Maybe Int) -> Maybe Int -> [[InterpSegment]] -> Maybe Int
    findMinIndent _ found [] = found
    findMinIndent f found ((seg:_):rest) =
      findMinIndent f (getMin <$> on mappend (fmap Min) (f seg) found) rest
    findMinIndent f found ([]:rest) = findMinIndent f found rest

reduceIndents :: Mindent -> Lines -> Lines
reduceIndents _ [] = []
reduceIndents i@(UsesSpaces indent) ((Spaces n:line):rest) =
  (Spaces (n-indent):line) : reduceIndents i rest
reduceIndents i@(UsesTabs indent) ((Tabs n:line):rest) =
  (Tabs (n-indent):line) : reduceIndents i rest
reduceIndents i (line:rest) = line : reduceIndents i rest

findMixedIndents :: Mindent -> Lines -> [IndentWarning]
findMixedIndents mindent = go
  where
    go :: [[InterpSegment]] -> [IndentWarning]
    go [] = []
    go (line:lines) = do
      let
        ind = indentation line
        warn = IndentWarning
          { indentLine = displayLine line, indentBase = mindent }
      case (mindent, any isSpaces ind, any isTabs ind) of
        (UsesSpaces _, _, True) -> warn : go lines
        (UsesTabs _, True, _)   -> warn : go lines
        _                       -> go lines

    indentation :: [InterpSegment] -> [InterpSegment]
    indentation =
      takeWhile (\case { Spaces _ -> True; Tabs _ -> True; _ -> False })

    isSpaces :: InterpSegment -> Bool
    isSpaces (Spaces n) = n > 0
    isSpaces _          = False

    isTabs :: InterpSegment -> Bool
    isTabs (Tabs n) = n > 0
    isTabs _        = False
