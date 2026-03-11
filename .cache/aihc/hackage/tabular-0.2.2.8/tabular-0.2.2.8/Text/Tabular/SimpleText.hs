module Text.Tabular.SimpleText where

import Data.List (intersperse, transpose)
import Text.Tabular

render :: String -- ^ delim
       -> (rh -> String)
       -> (ch -> String)
       -> (a -> String)
       -> Table rh ch a
       -> String
render delim fr fc f (Table rh ch cells) =
  unlines $ [ renderColumns delim ch2
            ] ++
            (renderRs $ fmap renderR $ zipHeader [] cells $ fmap fr rh)
 where
  -- ch2 and cell2 include the row and column labels
  ch2 = Group DoubleLine [Header "", fmap fc ch]
  cells2 = headerContents ch2
         : zipWith (\h cs -> h : map f cs) rhStrings cells
  --
  renderR (cs,h) = renderColumns delim $ Group DoubleLine
                    [ Header h
                    , fmap fst $ zipHeader "" (map f cs) ch]
  rhStrings = map fr $ headerContents rh
  renderRs (Header s)   = [s]
  renderRs (Group _ hs) = concatMap renderRs hs

renderColumns :: String
              -> Header String
              -> String
renderColumns delim h =
  concatMap helper $ flattenHeader h
 where
  helper = either (const delim) id
