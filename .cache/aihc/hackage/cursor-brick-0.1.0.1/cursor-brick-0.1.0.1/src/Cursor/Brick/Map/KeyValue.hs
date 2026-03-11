module Cursor.Brick.Map.KeyValue where

import Brick.Types
import Cursor.Map

keyValueWidget ::
  (kc -> v -> Widget n) ->
  (k -> vc -> Widget n) ->
  KeyValueCursor kc vc k v ->
  Widget n
keyValueWidget = foldKeyValueCursor

keyValueWidgetM ::
  (kc -> v -> f (Widget n)) ->
  (k -> vc -> f (Widget n)) ->
  KeyValueCursor kc vc k v ->
  f (Widget n)
keyValueWidgetM = foldKeyValueCursor
