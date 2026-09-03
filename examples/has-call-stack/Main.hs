module Main where

import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), callStack, getCallStack, withFrozenCallStack)

-- | Render each call site as @function:line:column@.
frames :: CallStack -> String
frames stack = joinFrames (getCallStack stack)
  where
    joinFrames [] = "<empty>"
    joinFrames [entry] = frame entry
    joinFrames (entry : entries) = frame entry ++ ", " ++ joinFrames entries
    frame (name, location) =
      name ++ ":" ++ show (srcLocStartLine location) ++ ":" ++ show (srcLocStartCol location)

report :: HasCallStack => String -> IO ()
report label = putStrLn (label ++ ": " ++ frames callStack)

outer :: HasCallStack => IO ()
outer = report "outer"

frozen :: HasCallStack => IO ()
frozen = withFrozenCallStack (report "frozen")

inferred :: IO ()
inferred = report "inferred"

main :: IO ()
main = do
  report "main"
  outer
  frozen
  inferred
