module Game.VierGewinnt.Text (
   spiel,
   ) where

import Game.VierGewinnt
   (Spieler(..), Zug, Spielstand, brettVon,
    anfangundzuege, berechneSpielstand, )

import Data.Array((!))


symbol :: Maybe Spieler -> Char
symbol Nothing         = ' '
symbol (Just Mensch)   = '@'
symbol (Just Computer) = '+'

--- ASCII-dump
spielstand :: Spielstand -> String
spielstand stand =
    show (anfangundzuege stand) ++ "\n" ++
    concatMap (++"|\n") (concat ([
      [ruler,line (7-j),nline (7-j)] | j<-[1..6] ])++[nruler])
      where
        brett = brettVon stand
        ruler, nruler  :: String
        line, nline  :: Int-> String
        ruler    = " "++ concat (replicate 7 ("|-----"))
        nruler   = " "++ concatMap (\i->"|--"++show i++"--") [1..(7::Int)]
        line j   = " "++ concatMap (\i->"|" ++
                            replicate 5 (symbol (brett!(i,j)))) [1..7]
        nline j  = show j ++ tail (line j)

-- We may use a custom Spielstand datatype in order to avoid the orphan instance
instance Show Spielstand where
    show = spielstand


spiel :: (Spieler,[Zug]) -> String
spiel = spielstand . berechneSpielstand
