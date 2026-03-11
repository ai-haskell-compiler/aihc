-- | Input mappings for Windows terminals 
-- pulled directly from https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences#input-sequences
module Graphics.Vty.Platform.Windows.Input.Terminfo.ANSIVT
  ( classifyTable
  )
where

import Graphics.Vty.Input.Events

-- | Encoding for all special keys
classifyTable :: [ClassifyMap]
classifyTable =
    [ cursorKeysMap
    , numpadAndF5ToF12
    , functionKeys1To4
    ]

-- | Encoding for cursor keys.
cursorKeysMap :: ClassifyMap
cursorKeysMap =
  [("\ESC[" ++ prefix mods ++ infx ++ name, EvKey key mods)
    | (name, key) <-
        [ ("A", KUp)
        , ("B", KDown)
        , ("C", KRight)
        , ("D", KLeft)
        , ("F", KEnd)
        , ("H", KHome)
        ]
    , (infx, mods) <- modMap
  ]
  where
    prefix mods = if null mods then "" else "1"

-- | encoding for numpad keys and F5 through F12
numpadAndF5ToF12 :: ClassifyMap
numpadAndF5ToF12 =
  [ ("\ESC[" ++ name ++ infx ++ "~", EvKey key mods)
    | (name, key) <-
        [ ("2", KIns)
        , ("3", KDel)
        , ("5", KPageUp)
        , ("6", KPageDown)
        , ("15", KFun 5)
        , ("17", KFun 6)
        , ("18", KFun 7)
        , ("19", KFun 8)
        , ("20", KFun 9)
        , ("21", KFun 10)
        , ("23", KFun 11)
        , ("24", KFun 12)
        ]
    , (infx, mods) <- modMap
  ]

-- | encoding for F1 through F4
functionKeys1To4 :: ClassifyMap
functionKeys1To4 =
  [ ("\ESC" ++ prefix mods ++ infx ++ name, EvKey key mods)
  | (name, key) <-
      [ ("P", KFun 1),
        ("Q", KFun 2),
        ("R", KFun 3),
        ("S", KFun 4)
      ]
  , (infx, mods) <- modMap
  ]
  where
    prefix mods = if null mods then "O" else "[1"

modMap :: [(String, [Modifier])]
modMap =
  [ ("", []),
    (";2", [MShift]),
    (";3", [MMeta]),
    (";4", [MMeta, MShift]),
    (";5", [MCtrl]),
    (";6", [MCtrl, MShift]),
    (";7", [MMeta, MCtrl]),
    (";8", [MMeta, MCtrl, MShift])
  ]