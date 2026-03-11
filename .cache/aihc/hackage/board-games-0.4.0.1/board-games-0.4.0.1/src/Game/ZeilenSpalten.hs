module Game.ZeilenSpalten (
   Koordinate,
   Spieler(..),
   Spielstand,
   Zug,
   berechneSpielstandUndZug,
   moeglicheZuege,
   amZug,
   grundstellung,
   koordinate,
   optimalerZug,
   punkte,
   spielfeld,
   zahlenfeld,

   spiel,
   optimalesSpiel,
   ) where

{-
  Benutzung in GHCi:
  Game.ZeilenSpalten> spiel (5,5) 123 Spalte [0,1,2]
-}

import qualified Game.Tree as GameTree

import Data.Array (Array, Ix, range, rangeSize, bounds, listArray, (!), (//), )
import Data.Maybe (isNothing, )
import Data.List  (intersperse, unfoldr, )
import System.Random (mkStdGen, randomR, randomRs, )


type Zahlenfeld = Array (Koordinate,Koordinate) Zahl
type Spielfeld  = Array (Koordinate,Koordinate) (Maybe Spieler)
type Zahl       = Int
type Punkte     = Int
type Zug        = Int
type Koordinate = Int

data Spielstand = S
   {punkte     :: (Punkte, Punkte),
    amZug      :: Spieler,
    koordinate :: Koordinate,
    zahlenfeld :: Zahlenfeld,
    spielfeld  :: Spielfeld}

data Spieler =
     Spalte
   | Zeile
   deriving (Show,Read,Eq,Ord,Ix)

spielerSymbol :: Spieler -> Char
spielerSymbol Spalte = '+'
spielerSymbol Zeile  = '@'

gegner :: Spieler -> Spieler
gegner Zeile  = Spalte
gegner Spalte = Zeile

punkteDifferenz :: Spielstand -> Punkte
punkteDifferenz s =
   uncurry (-) (punkte s)


moeglicheZuege :: Spielstand -> [(Zug, Spielstand)]
moeglicheZuege s =
   let k = koordinate s
       y = spielfeld s
       z = zahlenfeld s
       p = punkte s
       dran = amZug s
       ((i0,j0),(i1,j1)) = bounds z
       zuege =
          case dran of
             Spalte -> filter (isNothing . (y!) . (\j->(k,j))) [j0..j1]
             Zeile  -> filter (isNothing . (y!) . (\i->(i,k))) [i0..i1]
       poss =
          case dran of
             Spalte -> map (\j -> (k,j)) zuege
             Zeile  -> map (\i -> (i,k)) zuege
       pneu (pspalte,pzeile) d =
          case dran of
             Spalte -> (pspalte + d, pzeile)
             Zeile  -> (pspalte, pzeile + d)
       spielstandNeu zug pos =
          (zug, S (pneu p (z!pos)) (gegner dran) zug
                  z (y//[(pos, Just dran)]))
   in  zipWith spielstandNeu zuege poss

grundstellung :: (Koordinate,Koordinate) -> Int -> Spieler -> Spielstand
grundstellung (breite,hoehe) saat beginner =
   let groesse = ((0,0),(breite-1,hoehe-1))
       g = mkStdGen saat
       kgrenze = case beginner of
                    Zeile  -> hoehe
                    Spalte -> breite
       (k,g') = randomR (0,kgrenze-1) g
   in  S (0,0)
         beginner
         k
         (listArray groesse (randomRs (0,9) g'))
         (listArray groesse (repeat Nothing))

spielstandZuText :: Spielstand -> String
spielstandZuText s =
   let z = zahlenfeld s
       y = spielfeld s
       aktiv = koordinate s
       ((i0,j0),(i1,j1)) = bounds z
       symbol (i,j) =
          maybe (if (case amZug s of
                       Spalte -> i == aktiv
                       Zeile  -> j == aktiv)
                    then '?' else ' ')
                spielerSymbol (y!(i,j))
   in  unlines (
          concat ("    " : intersperse "  " (map show (range (i0,i1)))) :
          concat ("  +" : replicate (rangeSize (i0,i1)) "---") :
          map (\j ->
             show j ++ " | " ++
             unwords (map (\i -> show (z!(i,j)) ++ [symbol (i,j)])
                [i0..i1])) [j0..j1] ++
          ["Punkte (Spalter, Zeiler): " ++ show (punkte s) ++
           ", Differenz: " ++ show (punkteDifferenz s)])

instance Show Spielstand where
   show = spielstandZuText

{- Computer spielt begonnene Partie fuer beide Seiten optimal zu Ende -}
optimalesSpiel :: (Koordinate,Koordinate) -> Int -> Spieler -> [Zug] -> [Zug]
optimalesSpiel groesse saat beginner zuege =
   let spielbaum =
          GameTree.selectDeepSubTree zuege
             (GameTree.build moeglicheZuege
                (grundstellung groesse saat beginner))
   in  unfoldr (\s -> fmap (\zug -> (zug, GameTree.selectSubTree zug s))
                           (optimalerZug s))
               spielbaum

optimalerZug :: GameTree.T Zug Spielstand -> Maybe Zug
optimalerZug spielaktuell =
   let spielstand = GameTree.state spielaktuell
       besterZug =
          if True  -- schnell oder nicht
            then case amZug spielstand of
                    Spalte -> GameTree.maximumMoveFast
                    Zeile  -> GameTree.minimumMoveFast
            else case amZug spielstand of
                    Spalte -> GameTree.maximumMove
                    Zeile  -> GameTree.minimumMove
   in  if null (GameTree.subTrees spielaktuell)
         then Nothing
         else Just (besterZug (-- GameTree.pruneVolume 100000
                               GameTree.pruneDepth 4
                       {- bewerte Knoten fuer sofortiges Ende hoeher,
                          dadurch wird Gegner-K.O. forciert,
                          aber eigenes K.O. eher abgewendet. -}
                       (GameTree.mapNodesLeaves id (5*)
                       -- (GameTree.mapNodesLeaves id id
                           {- Wir wollen Zugfolge, die unseren Vorsprung maximiert. -}
                           (fmap (uncurry (-) . punkte) spielaktuell))))
                           {- Wir wollen Zugfolge mit den meisten Punkten fuer uns.
                              Unter allen Folgen mit gleicher Punktzahl fuer uns,
                              goennen wir dem Gegner die wenigsten. -}
                           -- (fmap ((\(ps,pz) -> (ps,-pz)) . punkte) spielaktuell))))


berechneSpielstandUndZug ::
   (Koordinate,Koordinate) -> Int -> Spieler -> [Zug] -> (Spielstand, Maybe Zug)
berechneSpielstandUndZug groesse saat beginner zuege =
   let spielbaum =
          GameTree.build moeglicheZuege
             (grundstellung groesse saat beginner)
       spielaktuell =
          GameTree.selectDeepSubTree zuege spielbaum
       spielstand = GameTree.state spielaktuell
   in  (spielstand, optimalerZug spielaktuell)


spielZuText :: (Koordinate,Koordinate) -> Int -> Spieler -> [Zug] -> String
spielZuText groesse saat beginner zuege =
   let (spielstand, optZug) =
          berechneSpielstandUndZug groesse saat beginner zuege
       zugvorschlag =
          maybe "Nix geht mehr."
             (\zug -> "Zugvorschlag f\252r " ++ show (amZug spielstand) ++ "r: " ++
                     show zug)
             optZug
   in  spielstandZuText spielstand
        ++ zugvorschlag ++ "\n"

spiel :: (Koordinate,Koordinate) -> Int -> Spieler -> [Zug] -> IO ()
spiel groesse saat beginner zuege =
   putStr (spielZuText groesse saat beginner zuege)
