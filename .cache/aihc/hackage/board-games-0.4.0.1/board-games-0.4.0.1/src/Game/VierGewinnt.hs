module Game.VierGewinnt (
   Spieler(..),
   Spielstand,
   Zug,
   anfangundzuege,
   berechneSpielstand,
   brettVon,

   grundstellung,
   istMatt,
   moeglicheZuege,
   wertung,
   ) where

{- <plaintext>

http://sim.mathematik.uni-halle.de/~helmut/Fun/Game.VierGewinnt/vierg.hs

VIER-GEWINNT IN HASKELL

Idee und Programmierung: Joerg Wensch und Helmut Podhaisky
                         {wensch,podhaisky}@mathematik.uni-halle.de
Version vom Mai 2002

Kopierrechte nach GPL
(GNU GENERAL PUBLIC LICENSE, siehe http://www.gnu.org/)

-}

import qualified Data.List.Key as Key

import Data.Array (Array, Ix, array, range, inRange, listArray, (!), (//), )
import Data.Maybe (isNothing, catMaybes, )

{-
Es gibt die Dateneinheit Spielstand. Zu ihr gehoert immer eine
Sofortwertung. Neue Zuege koennen nur mit doTheMove aus einem alten
Spielstand abgeleitet werden. So sind inkrementelle Bewertungsfunktionen
leicht implementierbar. Die Positionsbewertung erfolgt mit einem
(vorzeichenbehafteten) Integerwert. Alle Sonderfaelle lassen sich in
diesen Integerwert hineinpressen. Z.B. so:

 + posinfty >= matt in 7 => matt in 3 >= 500 Materialpunkte >= 10
 Materialpunkte >= patt in 10 >= patt in 1 >= sofortpatt >= - 10
 materialpunkte >= matt in 7 >= -neginfty

Besonderheiten fuer Vier-Gewinnt:
Es gibt 69 Vierer. Wir werten nur diese, unabhaengig von der konkreten
Lage mit 0 1 5 oder 50 Punkten fuer 0,1,2 oder 3 Steine (und Luft
sonst).


Das muss ein Fehler sein:
In folgender Spielsituation tut der Computer was ganz dummes,
er verhindert nicht den Vierer.
putStr (game (Computer, [1,4,4,4,6,3,3,7,3,4,5,6,3,3,1,6,2,2,2,4,5,2]))

-}

type Brett      = Array (Int,Int) (Maybe Spieler)
type Fuellstand = Array Int Int
type Wertung    = Int
type Zug        = Int
data Spieler =
     Computer
   | Mensch
   deriving (Show,Read,Eq,Ord,Ix)

{-
instance Ix a => Ix Maybe a where
a general Ix instance for Maybe cannot be defined,
because if the lower index bound is Nothing
then we do not know the next larger index of the form (Just i).
This is a problem e.g. for Maybe Int
-}

data Spielstand =
      S {brettVon       :: Brett,
         fuellstandVon  :: Fuellstand,
         amZug          :: Spieler,
         wertung        :: Wertung,
         anfangundzuege :: (Spieler, [Zug])}

brettGroesse :: ((Int,Int),(Int,Int))
brettGroesse = ((1,1),(7,6))

posinfty, neginfty :: Wertung
posinfty =  10000
neginfty = -10000

gegner :: Spieler -> Spieler
gegner Mensch   = Computer
gegner Computer = Mensch



moeglicheZuege :: Spielstand -> [Int]
moeglicheZuege spielstand =
    let brett = brettVon spielstand
    in  filter (\i -> isNothing (brett!(i,6))) [1..7]

computerAmZug :: Spielstand -> Bool
computerAmZug spielstand =
    amZug spielstand == Computer

istMatt, istPatt :: Spielstand -> Bool
istMatt spielstand = abs (wertung spielstand) >= 5000
istPatt spielstand = null (moeglicheZuege spielstand)

doTheMove :: Spielstand->Zug->Spielstand
doTheMove spielstand zug =
    S neuesBrett neuerFuellstand derGegner neueWertung (anfang,zuege)
    where
    altesBrett,neuesBrett :: Brett
    altesBrett = brettVon spielstand
    alterFuellstand = fuellstandVon spielstand
    drann = amZug spielstand
    derGegner = gegner drann
    alteWertung = wertung spielstand
    neueWertung = updateWertung alteWertung altesBrett neuesBrett (zug,j)
    neuesBrett=altesBrett // (((zug,j),Just drann):[])
    neuerFuellstand=alterFuellstand // ((zug,j):[])
    j=alterFuellstand!zug + 1
    (anfang,altezuege)= anfangundzuege spielstand
    zuege=altezuege++(zug:[])

_fmtWertung :: Wertung -> String
_fmtWertung w =
    if -5000 < w && w < 5000
      then show w
      else " Matt in " ++ show (6000 - abs w)

updateWertung ::  Int -> Brett-> Brett -> (Int,Int) -> Wertung
updateWertung alteWertung altesBrett neuesBrett (i,j)
      = neueWertung
    where
    aktiv = aktiveVierer!(i,j)
    alt = sum (map (werte . map (altesBrett!)) aktiv)
    neu = sum (map (werte . map (neuesBrett!)) aktiv)

    neueWertung :: Wertung
    neueWertung =
        if abs neu >= 5000
          then maxmin neu
          else maxmin (alteWertung + neu - alt)

    maxmin x = max (min x 6000) (-6000)
    w :: Int -> Int
    w x = [0,1,5,50,posinfty] !! x

    werte xs =
       let xsm = catMaybes xs
           {- effizienter?
           steine = accumArray (+) 0 (Computer, Mensch)
                               (map (\x->(x,1)) xsm)
           computerSteine = steine ! Computer
           menschSteine   = steine ! Mensch
           -}
           computerSteine = length (filter (Computer ==) xsm)
           menschSteine   = length (filter (Mensch   ==) xsm)
       in  if computerSteine>0 && menschSteine>0
             then 0
             else w computerSteine - w menschSteine



aktiveVierer :: Array (Int,Int) [[(Int,Int)]]
aktiveVierer = array brettGroesse
    (map (\ix -> (ix, uncurry viererVon ix)) (range brettGroesse))

viererVon :: Int -> Int -> [[(Int, Int)]]
viererVon i j = filter (elem (i,j)) alleVierer
alleVierer :: [[(Int,Int)]]
alleVierer = [ [(i+l*ii,j+l*jj) | l<-[0..3] ]
                 | (i,j)   <- range brettGroesse,
                   (ii,jj) <- inc i j ]
    where
    inc i j = filter (\(ii,jj) ->
                         inRange brettGroesse (i+3*ii,j+3*jj))
                     [(0,1),(1,-1),(1,0),(1,1)]

grundstellung :: Spieler -> Spielstand
grundstellung x = S leeresBrett leererFuellstand x 0 (x,[])
    where
    leeresBrett=listArray brettGroesse (repeat Nothing)
    leererFuellstand=listArray (1,7) (repeat 0)

-- Korrektur der Matt-Vorhersage-Zuege
bewertungsKorrektur :: Wertung->Wertung
bewertungsKorrektur w
    | w > 5000  = w-1
    | w < -5000 = w+1
    | otherwise = w

{-
Wir berechnen die Stellungsbewertung zum gegebenen Brett mit einer
vorgegebenen Tiefe einschliesslich der Zuege zu dieser Stellung.

----------------------
Alpha-Beta-Suche:

Ebene 0            X
Ebene 1     A      B        C
Ebene 2   a  b    c d e     ...

wert(X)=max(A,B,C)
wert(A)=min(a,b)
wert(B)=min(c,d,e)

Wir moechten nun den Wert von B abschaetzen. D.h. wir wollen wissen,
ob wert(B)<= wert(A) ist. Das ist bereits dann der Fall, wenn
wert(c)<=wert(A) ist. Dann brauchen wir d und e gar nicht mehr
anzusehen, denn es gilt wert(X) >= max (A, min (c,d,e)).

Auswerten mit Schranke:

w(A)            = max (-inf, min (a,b))
wert(B) <= w(B) = max (w(A), min (c,d,e))
w(C)            = max (W(B), min (...)

scanl (\x,y->f(x,X)) w(A) [B,C,D] = [w(a),f(w(a),B),f(f(w(a),B),C) ...]
also: f=(\x,y-> max (x,min(kinder y)))

----------------------

Weil ein Matt in 5 schlechter als ein Matt in 3 ist, muss beim
Zusammensetzen der Wertung vom Tochterknoten zum Mutterknoten u.U. eine
Bewertungskorrektur durchgefuehrt werden.

-}

tiefe0 :: Int
tiefe0 = 0

search :: Int->Spielstand->(Wertung,[Zug])
search suchTiefe spielstand =
    bewerteKnoten suchTiefe tiefe0 posinfty spielstand


bewerteKnoten :: Int->Int->Int->Spielstand->(Wertung,[Zug])
bewerteKnoten suchTiefe tiefe bound spielstand
    |  tiefe == suchTiefe  = (wertung spielstand,[])
    |  istMatt spielstand = (wertung spielstand,[])
    |  istPatt spielstand = (0,[])
    |  tiefe < suchTiefe = bestesKind
    |  otherwise = error "bewerteKnoten: unmoeglicher Fall"
          where
          zuege1 = moeglicheZuege spielstand
          kinder1= map (doTheMove spielstand) zuege1

          -- Den jeweils besten Zug zuerst ausprobieren, dann
          -- fallen viele Faelle spaeter schneller weg.
          (kinder,zuege) = unzip
             ((if computerAmZug spielstand
                 then reverse
                 else id)
              (Key.sort (wertung . fst) (zip kinder1 zuege1)))

          godown ::  Int -> Spielstand -> (Wertung,[Zug])
          godown=bewerteKnoten suchTiefe (tiefe+1)
          godown1 (w,_)  = godown w

          wkind1=godown newbound (head kinder)
          werteKinder = scanl (godown1)  wkind1 (tail kinder)

          paareWertZug = zip werteKinder zuege
          bestesKind = verbinden (minmax' paareWertZug)

          (minmax,newbound) =
              if computerAmZug spielstand
                then (maximum,neginfty)
                else (minimum,posinfty)


          worseThan bound0 =
              if computerAmZug spielstand
                then bound0 + 5
                else bound0 - 5

          rel :: Ord a => a->a->Bool
          rel =
              if computerAmZug spielstand
                then (>)
                else (<)

          minmax' paareWertZug0 =
            if any (\x->rel (fst (fst x)) (bewertungsKorrektur bound))
                   paareWertZug0
              then ((worseThan (bewertungsKorrektur bound),[]),0)
              else minmax paareWertZug0

          verbinden ((wert,zuege0),zug)=(bewertungsKorrektur wert,zug:zuege0)
------------------------------------------------------

suchTiefeGesamt :: Int
suchTiefeGesamt = 6

berechneSpielstand :: (Spieler,[Zug]) -> Spielstand
berechneSpielstand (erster,zuege) =
    let spielstand = foldl doTheMove (grundstellung erster) zuege
    in  if istMatt spielstand || not (computerAmZug spielstand)
          then spielstand
          else doTheMove spielstand
                 (head $ snd (search suchTiefeGesamt spielstand))
