module Sound.MED.Basic.Pretty where

pretty :: String -> String
pretty = pretty' 0

pretty' :: Int -> String -> String
pretty' _     []     = "\n"
pretty' level (c:cs) = case c of
  ' ' -> pretty' level cs
  '=' -> " = " ++ pretty' level cs
  '{' -> "{\n" ++ indent (level+1) ++        pretty' (level+1) cs
  '}' -> "\n"  ++ indent (level-1) ++ "}" ++ pretty' (level-1) cs
  '[' -> "[\n" ++ indent (level+1) ++        pretty' (level+1) cs
  ']' -> "\n"  ++ indent (level-1) ++ "]" ++ pretty' (level-1) cs
  '(' -> "(\n" ++ indent (level+1) ++        pretty' (level+1) cs
  ')' -> "\n"  ++ indent (level-1) ++ ")" ++ pretty' (level-1) cs
  ',' -> ",\n" ++ indent level ++ pretty' level cs
  _ -> c : pretty' level cs

indent :: Int -> String
indent level = replicate (level*4) ' '
