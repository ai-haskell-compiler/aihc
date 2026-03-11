module Graphics.Gnuplot.Private.Terminal where

data T =
   Cons {
      precommands :: [String],
      options :: [String],
      commands :: [String],
      interactive :: Bool
   }

class C terminal where
   canonical :: terminal -> T

format :: T -> [String]
format (Cons pre opts cmds _ia) =
   pre ++
   if null opts
     then cmds
     else (unwords $ "set" : "terminal" : opts) : cmds
