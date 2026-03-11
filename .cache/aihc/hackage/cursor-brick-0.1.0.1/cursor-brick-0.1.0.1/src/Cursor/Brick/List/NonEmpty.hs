module Cursor.Brick.List.NonEmpty where

import Brick.Types as B
import Brick.Widgets.Core as B
import Cursor.List.NonEmpty
import Data.List

verticalNonEmptyCursorTableWithHeader ::
  (b -> [Widget n]) -> (a -> [Widget n]) -> (b -> [Widget n]) -> [Widget n] -> NonEmptyCursor a b -> Widget n
verticalNonEmptyCursorTableWithHeader prevFunc curFunc nextFunc header =
  nonEmptyCursorWidget (\ps c ns -> tableWidget $ header : (map prevFunc ps ++ [curFunc c] ++ map nextFunc ns))

verticalNonEmptyCursorTableWithHeaderM ::
  Applicative f =>
  (b -> f [Widget n]) ->
  (a -> f [Widget n]) ->
  (b -> f [Widget n]) ->
  [Widget n] ->
  NonEmptyCursor a b ->
  f (Widget n)
verticalNonEmptyCursorTableWithHeaderM prevFunc curFunc nextFunc header =
  nonEmptyCursorWidgetM (\ps c ns -> tableWidget <$> sequenceA (pure header : (map prevFunc ps ++ [curFunc c] ++ map nextFunc ns)))

verticalNonEmptyCursorTable ::
  (b -> [Widget n]) -> (a -> [Widget n]) -> (b -> [Widget n]) -> NonEmptyCursor a b -> Widget n
verticalNonEmptyCursorTable prevFunc curFunc nextFunc =
  nonEmptyCursorWidget
    ( \ps c ns ->
        tableWidget $ map prevFunc ps ++ [curFunc c] ++ map nextFunc ns
    )

verticalNonEmptyCursorTableM ::
  Applicative f =>
  (b -> f [Widget n]) ->
  (a -> f [Widget n]) ->
  (b -> f [Widget n]) ->
  NonEmptyCursor a b ->
  f (Widget n)
verticalNonEmptyCursorTableM prevFunc curFunc nextFunc =
  nonEmptyCursorWidgetM (\ps c ns -> tableWidget <$> sequenceA (map prevFunc ps ++ [curFunc c] ++ map nextFunc ns))

tableWidget :: [[Widget n]] -> Widget n
tableWidget = hBox . intersperse (str " ") . map vBox . transpose

horizontalNonEmptyCursorWidgetM ::
  Applicative f =>
  (b -> f (Widget n)) ->
  (a -> f (Widget n)) ->
  (b -> f (Widget n)) ->
  NonEmptyCursor a b ->
  f (Widget n)
horizontalNonEmptyCursorWidgetM beforeFunc curFunc afterFunc =
  nonEmptyCursorWidgetM $ \befores current afters ->
    hBox
      <$> sequenceA
        (concat [map beforeFunc befores, [curFunc current], map afterFunc afters])

horizontalNonEmptyCursorWidget ::
  (b -> Widget n) ->
  (a -> Widget n) ->
  (b -> Widget n) ->
  NonEmptyCursor a b ->
  Widget n
horizontalNonEmptyCursorWidget beforeFunc curFunc afterFunc =
  nonEmptyCursorWidget $ \befores current afters ->
    hBox $
      concat [map beforeFunc befores, [curFunc current], map afterFunc afters]

verticalNonEmptyCursorWidgetM ::
  Applicative f =>
  (b -> f (Widget n)) ->
  (a -> f (Widget n)) ->
  (b -> f (Widget n)) ->
  NonEmptyCursor a b ->
  f (Widget n)
verticalNonEmptyCursorWidgetM beforeFunc curFunc afterFunc =
  nonEmptyCursorWidgetM $ \befores current afters ->
    vBox
      <$> sequenceA
        (concat [map beforeFunc befores, [curFunc current], map afterFunc afters])

verticalNonEmptyCursorWidget ::
  (b -> Widget n) ->
  (a -> Widget n) ->
  (b -> Widget n) ->
  NonEmptyCursor a b ->
  Widget n
verticalNonEmptyCursorWidget beforeFunc curFunc afterFunc =
  nonEmptyCursorWidget $ \befores current afters ->
    vBox $
      concat [map beforeFunc befores, [curFunc current], map afterFunc afters]

nonEmptyCursorWidgetM ::
  ([b] -> a -> [b] -> m (Widget n)) -> NonEmptyCursor a b -> m (Widget n)
nonEmptyCursorWidgetM = foldNonEmptyCursor

nonEmptyCursorWidget ::
  ([b] -> a -> [b] -> Widget n) -> NonEmptyCursor a b -> Widget n
nonEmptyCursorWidget = foldNonEmptyCursor
