-- | Formats Haskell source code using mIRC codes.
--   (see http:\/\/irssi.org\/documentation\/formats)
module Language.Haskell.HsColour.MIRC (hscolour) where

import Language.Haskell.HsColour.Classify as Classify
import Language.Haskell.HsColour.Colourise

import Data.Char(isAlphaNum)


-- | Formats Haskell source code using mIRC codes.
hscolour :: ColourPrefs -- ^ Colour preferences.
         -> String      -- ^ Haskell source code.
         -> String      -- ^ Coloured Haskell source code.
hscolour pref = concatMap (renderToken pref) . tokenise

renderToken :: ColourPrefs -> (TokenType,String) -> String
renderToken pref (t,s) = fontify (colourise pref t) s


-- mIRC stuff
fontify hs =
    mircColours (joinColours hs)
    . highlight (filter (`elem`[Normal,Bold,Underscore,ReverseVideo]) hs)
  where
    highlight [] s     = s
    highlight (h:hs) s = font h (highlight hs s)

    font Normal         s = s
    font Bold           s = '\^B':s++"\^B"
    font Underscore     s = '\^_':s++"\^_"
    font ReverseVideo   s = '\^V':s++"\^V"

-- mIRC combines colour codes in a non-modular way
data MircColour = Mirc { fg::Colour, dim::Bool, bg::Maybe Colour, blink::Bool}

joinColours :: [Highlight] -> MircColour
joinColours = foldr join (Mirc {fg=Black, dim=False, bg=Nothing, blink=False})
  where
    join Blink           mirc = mirc {blink=True}
    join Dim             mirc = mirc {dim=True}
    join (Foreground fg) mirc = mirc {fg=fg}
    join (Background bg) mirc = mirc {bg=Just bg}
    join Concealed       mirc = mirc {fg=Black, bg=Just Black}
    join _               mirc = mirc

mircColours :: MircColour -> String -> String
mircColours (Mirc fg dim Nothing   blink) s = '\^C': code fg dim++s++"\^O"
mircColours (Mirc fg dim (Just bg) blink) s = '\^C': code fg dim++','
                                                   : code bg blink++s++"\^O"

code :: Colour -> Bool -> String
code Black   False = "01"
code Red     False = "05"
code Green   False = "03"
code Yellow  False = "07"
code Blue    False = "02"
code Magenta False = "06"
code Cyan    False = "10"
code White   False = "00"
code Black   True  = "14"
code Red     True  = "04"
code Green   True  = "09"
code Yellow  True  = "08"
code Blue    True  = "12"
code Magenta True  = "13"
code Cyan    True  = "11"
code White   True  = "15"
code c@(Rgb _ _ _) b = code (projectToBasicColour8 c) b
