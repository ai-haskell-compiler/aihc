{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Language.Haskell.TH

-- A quote in a module that also has a do block in an operand of an infix
-- expression. Name resolution runs two times on such a module, and the
-- second time must not see the annotations of the first time.
triple :: Q Exp
triple = [|\x -> (x, 'c', "s")|]

withUnit :: (() -> IO a) -> IO a
withUnit action = action ()

-- The program builds the syntax tree of the quote when it runs.
showTriple :: IO ()
showTriple =
  withUnit $ \_ -> do
    tree <- runQ triple
    print tree

main :: IO ()
main = do
  -- Splices that build the syntax tree with functions.
  print ($(litE (integerL 42)) :: Int)
  print ($(appE (varE (mkName "negate")) (litE (integerL 7))) + 1 :: Int)
  putStrLn $(litE (stringL (concat (replicate 3 "ab"))))
  -- Splices that build the syntax tree with quotes.
  print ($([|length "abc" + 1|]) :: Int)
  -- The quote lifts the value of a variable from outside the quote.
  print ($(let n = 5 :: Int in [|n * 2|]) :: Int)
  print ($([|\x -> x + 1|]) (41 :: Int))
  -- The name in the quote keeps its meaning where a local name hides it.
  let length = const (0 :: Int) in print ($([|length "abcd"|]) :: Int)
  print $([|(if True then 'a' else 'b', [length "x", 2])|])
  showTriple
