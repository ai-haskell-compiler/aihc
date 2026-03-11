{-# Language ScopedTypeVariables #-}
module Numeric.MathExpr
    ( evaluate
    , Settings (..)
    , defaultFunctions
    , defaultOperators
    ) where
      import Data.Default.Class
      import Data.Maybe (isJust, fromJust)
      import Data.List (find)

      -- | Operators are in the form (character, precedence, function)
      -- Example: ('+', 0, (+)), ('', 1, ())
      -- (higher the precedence, the sooner the operator operates)
      --
      -- Functions are in the form (name, function)
      -- Example: ("ln", log)
      data Settings = Settings { operators :: [(Char, Int, Double -> Double -> Double)]
                               , functions :: [(String, Double -> Double)]
                               }
      -- | Operators are in the form (character, precedence, function)
      -- Example: ('+', 0, (+)), ('', 1, ())
      -- (higher the precedence, the sooner the operator operates)
      defaultOperators = [
          ('+', 0, (+)), ('-', 0, (-)),
          ('*', 1, (*)), ('/', 1, (/)),
          ('^', 2, (**))
        ]

      -- | Functions are in the form (name, function)
      -- Example: ("ln", log)
      defaultFunctions = [("ln", log), ("sin", sin), ("cos", cos)]

      instance Default Settings where
        def = Settings { operators = defaultOperators
                       , functions = defaultFunctions
                       }
      toPostfix :: Settings -> String -> String
      toPostfix settings s = helper (tokenize s) [] []
        where
          ops = operators settings
          fns = functions settings

          helper :: [String] -> [String] -> String -> String
          helper [] os out = out ++ concat os
          helper (c:cs) os out
            | head c == '(' = helper cs (c:os) out
            | head c == ')' && head os == "(" = helper cs (tail os) out
            | head c == ')' = helper (c:cs) (tail os) (out ++ pad (head os))
            | isOperator c && (null os || precedence c > precedence (head os)) = helper cs (c:os) out
            | isOperator c = helper (c:cs) (tail os) (out ++ pad (head os))
            | otherwise = helper cs os (out ++ pad c)

          isOperator cs = isOp cs || isFunction cs
          isOp cs = isJust $ (head cs) `triLookup` ops
          isFunction cs = isJust $ cs `lookup` fns
          precedence cs
            | isFunction cs = Just 999
            | otherwise = (head cs) `triLookup` ops

      tokenize :: String -> [String]
      tokenize str = words $ helper str
        where
          helper :: String -> String
          helper [] = []
          helper (c:cs)
            | isAlphanumeric c = c : helper cs
            | isSymbol c = pad [c] ++ helper cs

      replaceVariables :: String -> [(String, Double)] -> String
      replaceVariables str [] = str
      replaceVariables str vars = concatMap replace (tokenize str)
        where
          replace c
            | isVariable c = pad $ show $ fromJust $ c `lookup` vars
            | otherwise = pad c

          isVariable c = isJust $ c `lookup` vars

      -- | Evaluate an expression
      -- Example: `evaluate def "x + y ^ 2" [("x", 1), ("y", 2)]
      evaluate :: Settings -> String -> [(String, Double)] -> Double
      evaluate settings expr vars =
        let postfix = toPostfix settings expr
            replaced = replaceVariables postfix vars
        in helper (tokenize replaced) []
        where
          ops = operators settings
          fns = functions settings

          helper :: [String] -> [String] -> Double
          -- negative numbers come in the form ["num", "-"]
          helper [] [o] = read o
          helper [n, "-"] [] = negate . read $ n
          helper ["-"] [n] = negate . read $ n
          helper (c:cs) os
            | c == "-" && length os < 2 =
                let result :: Double = negate . read . head $ cs
                in helper (tail cs) $ (show result) : os
            | isOperator c && length os >= 2 =
                let result = (operatorFunction c) (read . head . tail $ os) (read . head $ os) 
                in helper cs $ (show result) : drop 2 os
            | isFunction c =
                let result = (function c) (read . head $ os)
                in helper cs $ (show result) : tail os
            | otherwise = helper cs (c:os)

          isOperator cs = isJust $ (head cs) `triLookup` ops
          isFunction cs = isJust $ cs `lookup` fns

          function cs = fromJust $ cs `lookup` fns
          operatorFunction cs = case find (\(a, _, _) -> a == head cs) ops of
                                  Just (_, _, c) -> c
                                  Nothing -> const (const 0)
          isParen cs = head cs `elem` ['(', ')']

      alphanumeric = '.' : ['a'..'z'] ++ ['0'..'9']
      isAlphanumeric = (`elem` alphanumeric)
      isSymbol = not . (`elem` alphanumeric)  

      triLookup :: (Eq a) => a -> [(a, b, c)] -> Maybe b
      triLookup a x = lookup a $ map (\(a, b, _) -> (a, b)) x

      pad :: String -> String
      pad x = ' ' : x ++ [' ']
