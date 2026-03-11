module Graphics.Gnuplot.Terminal.PostScript (
   T, cons,
   encoding,
   landscape, portrait, eps,
   color, monochrome,
   font, embedFont,
   ) where

import qualified Graphics.Gnuplot.Private.Terminal as Terminal
import qualified Graphics.Gnuplot.Private.Encoding as Encoding
import Graphics.Gnuplot.Utility (listFromMaybeWith, quote, )
import Data.Foldable (foldMap, )


data T =
   Cons {
      filename_ :: FilePath,
      encoding_ :: Maybe Encoding.T,
      mode_ :: Maybe Mode,
      color_ :: Maybe Bool,
      embedFont_ :: [FilePath],
      font_ :: Maybe (String, Int)
   }

cons :: FilePath -> T
cons path =
   Cons {
      filename_ = path,
      encoding_ = Nothing,
      mode_ = Nothing,
      color_ = Nothing,
      embedFont_ = [],
      font_ = Nothing
   }


{- |
Setting the encoding to anything different
from 'Graphics.Gnuplot.Encoding.locale'
makes only sense if you write your gnuplot files manually using this encoding.
-}
encoding :: Encoding.T -> T -> T
encoding enc term = term{encoding_ = Just enc}

landscape :: T -> T
landscape = setMode Landscape

portrait :: T -> T
portrait = setMode Portrait

eps :: T -> T
eps = setMode EPS


color :: T -> T
color term =
   term{color_ = Just True}

monochrome :: T -> T
monochrome term =
   term{color_ = Just False}

font :: String -> Int -> T -> T
font fontName fontSize term =
   term{font_ = Just (fontName, fontSize)}

{- |
Embed a font file in the generated PostScript output.
Each call adds a new font file,
there is no way to remove it again.
-}
embedFont :: FilePath -> T -> T
embedFont fontFile term =
   term{embedFont_ = fontFile : embedFont_ term}


-- private functions

data Mode =
     Landscape
   | Portrait
   | EPS

formatMode :: Mode -> String
formatMode mode =
   case mode of
      Landscape -> "landscape"
      Portrait  -> "portrait"
      EPS       -> "eps"

setMode :: Mode -> T -> T
setMode mode term = term{mode_ = Just mode}


instance Terminal.C T where
   canonical term =
      Terminal.Cons {
         Terminal.precommands = Encoding.formatMaybe $ encoding_ term,
         Terminal.options =
            "postscript" :
            (listFromMaybeWith formatMode $ mode_ term) ++
            (listFromMaybeWith (\b -> if b then "color" else "monochrome") $ color_ term) ++
            (concatMap (\path -> "fontfile" : quote path : []) $ embedFont_ term) ++
            (foldMap (\(name,size) -> "font" : quote name : show size : []) $ font_ term) ++
            [],
         Terminal.commands =
            ["set output " ++ (quote $ filename_ term)],
         Terminal.interactive = False
      }
