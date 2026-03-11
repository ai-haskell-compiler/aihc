module WQO where

import Language.REST.Internal.WQO as WQO
import Data.Maybe (isNothing)

basicInvalid :: Maybe (WQO Char)
basicInvalid = do
  wqo1 <- WQO.singleton ('A', 'C', QGT)
  WQO.insertMaybe wqo1 ('C', 'A', QGT)

tests :: [(String, Bool)]
tests =
  let
    ValidExtension fg   = insert empty ("f", "g", QGT)
    ValidExtension fgyz = insert fg ("y", "z", QGT)
  in
    [ ("NotStrongerThan", fg `notStrongerThan` fgyz)
    , ("RejectInvalid", isNothing basicInvalid)
    ]
