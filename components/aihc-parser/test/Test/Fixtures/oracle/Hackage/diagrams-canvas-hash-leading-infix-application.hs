{- ORACLE_TEST pass -}
module DiagramsCanvasCanvas where

infixl 0 #
(#) :: a -> (a -> b) -> b
(#) = flip id

adjustDia :: Int -> Int -> Int -> Int -> Int
adjustDia c opts d =
  adjustDia2D
    size
    c
    opts
    (d
     # reflectY)
  where
    size :: Int
    size = 42
    adjustDia2D :: Int -> Int -> Int -> Int -> Int
    adjustDia2D s p q r = s + p + q + r
    reflectY :: Int -> Int
    reflectY = id
