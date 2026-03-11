{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Format.TH ( formatQQ, format1QQ ) where

import           Data.String
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Format.ArgFmt
import           Text.Format.ArgKey
import           Text.Format.Format

deriving instance Lift ArgKey
deriving instance Lift ArgFmt
deriving instance Lift FmtAlign
deriving instance Lift FmtSign
deriving instance Lift FmtNumSep
deriving instance Lift FmtItem
deriving instance Lift Format
deriving instance Lift Format1


{-| A QuasiQuoter for 'Format' with which you can write multi-line 'Format'.

Note: @">>>"@ after @"[formatQQ|"@ means starting from the next line,
      @"<<<"@ before @"|]"@ means ending from the previous line.

==== Example

>>> :set -XTemplateHaskell
>>> :set -XQuasiQuotes
>>> import     Text.Format
>>> import     Text.Format.TH
>>> :{
fmt1 :: Format
fmt1 = [formatQQ|>>>
first line {hi}
newline {words}
last line {bye}
<<<|]
fmt2 :: Format
fmt2 = [formatQQ|first line {hi}
newline {words}
last line {bye}|]
fmt3 :: Format
fmt3 = "first line {hi}\nnewline {words}\nlast line {bye}"
:}
>>> format fmt1 ("hi" := "hi") ("words"  := "say something") ("bye" := "bye")
"first line hi\nnewline say something\nlast line bye"
>>> format fmt2 ("hi" := "hi") ("words"  := "say something") ("bye" := "bye")
"first line hi\nnewline say something\nlast line bye"
>>> format fmt3 ("hi" := "hi") ("words"  := "say something") ("bye" := "bye")
"first line hi\nnewline say something\nlast line bye"

@since 0.14.0
-}
formatQQ :: QuasiQuoter
formatQQ = QuasiQuoter { quoteExp = formatExp
                       , quotePat = undefined
                       , quoteType = undefined
                       , quoteDec = undefined
                       }

formatExp :: String -> Q Exp
formatExp fs = lift $ (fromString (fixEnd $ fixBegin fs) :: Format)


{-| Same as 'formatQQ', but for 'Format1'.

@since 0.14.0
-}
format1QQ :: QuasiQuoter
format1QQ = QuasiQuoter { quoteExp = format1Exp
                        , quotePat = undefined
                        , quoteType = undefined
                        , quoteDec = undefined
                        }

format1Exp :: String -> Q Exp
format1Exp fs = lift $ (fromString (fixEnd $ fixBegin fs) :: Format1)


fixBegin :: String -> String
fixBegin ('>':'>':'>':"")      = ""
fixBegin ('>':'>':'>':'\n':fs) = fs
fixBegin fs                    = fs


fixEnd :: String -> String
fixEnd ('<':'<':'<':"") = ""
fixEnd fs               | ('<':'<':'<':'\n':fs') <- reverse fs = reverse fs'
fixEnd fs               = fs
