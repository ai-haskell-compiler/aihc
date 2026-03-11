module Graphics.Gnuplot.Terminal.X11 (
   T, cons,
   title, noTitle,
   persist, noPersist,
   ) where

import qualified Graphics.Gnuplot.Private.Terminal as Terminal
import Data.Maybe (catMaybes, )
import Graphics.Gnuplot.Utility (quote, formatBool, )


data T =
   Cons {
      title_ :: Maybe String,
      persist_ :: Maybe Bool
   }

cons :: T
cons =
   Cons {
      title_ = Nothing,
      persist_ = Nothing
   }


title :: String -> T -> T
title text term = term{title_ = Just text}

noTitle :: T -> T
noTitle term = term{title_ = Nothing}

persist, noPersist :: T -> T
persist   term = term{persist_ = Just True}
noPersist term = term{persist_ = Just False}


instance Terminal.C T where
   canonical term =
      Terminal.Cons {
         Terminal.precommands = [],
         Terminal.options =
            "x11" :
            catMaybes (
               (fmap quote $ title_ term) :
               (fmap (formatBool "persist") $ persist_ term) :
               []),
         Terminal.commands = [],
         Terminal.interactive = True
      }
