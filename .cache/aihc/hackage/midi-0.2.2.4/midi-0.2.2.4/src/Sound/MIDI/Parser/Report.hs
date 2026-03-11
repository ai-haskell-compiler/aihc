{- |
Definition of a datatype that reports on the success of a parser.
-}
module Sound.MIDI.Parser.Report where


{- |
This datatype is the result of a parser.
First it stores a sequence of warnings.
Warnings are for corruptions of the input which can be fixed.
After encountering a series of warnings,
there is finally an end,
either a successful one, with the result as @(Right result)@
or an eventual non-fixable problem indicated by @(Left errorMessage)@.
-}
data T a =
   Cons {
      warnings :: [UserMessage],
      result   :: Either UserMessage a
   }
   deriving (Show, Eq)

type UserMessage = String
