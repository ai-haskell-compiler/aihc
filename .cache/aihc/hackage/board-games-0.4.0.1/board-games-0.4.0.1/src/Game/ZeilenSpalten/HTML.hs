{- | Ein- und Ausgabe fuer Game.ZeilenSpalten getrennt von Zugberechnung -}
module Game.ZeilenSpalten.HTML (
   komplett,
   erzeuge,
   main,
   ) where

import Game.ZeilenSpalten hiding (spiel)
import qualified Game.Tree as GameTree
import Game.Utility (nullToMaybe)

import qualified Text.Html as Html
import Text.Html((<<), (+++), concatHtml, toHtml)
import Text.Read.HT (maybeRead)

import qualified Network.CGI as CGI

import qualified Data.List as List
import Data.Array ((!), bounds, )
import Data.Maybe (maybeToList, isNothing, )
import Control.Monad (guard, )
import System.Random (randomIO, )


spielerFarbe :: Spieler -> String
spielerFarbe Zeile    = Html.blue
spielerFarbe Spalte   = Html.red

labelAnchor :: String -> Html.Html -> Html.Html
labelAnchor ref label =
   Html.anchor label Html.! [Html.href ref]

type Beschreibung = ((Koordinate,Koordinate), Int, Spieler, Bool, [Zug])

spielstand :: Beschreibung -> Spielstand -> Html.Html
spielstand (groesse, saat, beginner, _, zuege) s =
   let z = zahlenfeld s
       y = spielfeld s
       (ps,pz) = punkte s
       aktiv = koordinate s
       ((i0,j0),(i1,j1)) = bounds z
       aktivesFeld (i,j) = variableKoordinate (j,i) == aktiv
       variableKoordinate (i,j) =
          case amZug s of
             Spalte -> j
             Zeile  -> i
       anc ij =
          labelAnchor ("ZeilenSpalten?" ++
                erzeugeAnfrage
                   (groesse, saat, beginner, True,
                    zuege ++ [variableKoordinate ij]))
       farbe grund ij =
          maybe grund spielerFarbe (y!ij)
       feld =
          Html.table
              (concatHtml $ map (\j -> Html.tr
                 (concatHtml $ map (\i ->
                    let ij   = (i,j)
                        af   = aktivesFeld ij
                        fb   = if af then Html.yellow else Html.white
                        canc = if af && isNothing (y!ij) then anc ij else id
                        zahl = toHtml (show (z!ij))
                    in Html.td (canc zahl)
                              Html.! [Html.bgcolor (farbe fb ij)])
                  [i0..i1]))
               [j0..j1])
            Html.! [Html.border 2]
   in  Html.center
         (Html.simpleTable [] [{-Html.valign "top"-}]
            [[toHtml ("Spalter: " ++ show ps),
              feld,
              toHtml ("Zeiler: " ++ show pz)]])


-- umstaendlich, weil Spielfeld zweimal aufgebaut wird
_spiel :: String -> Html.Html
_spiel s =
   let (groesse, saat, beginner, macheGegenzug, zuege) = read s
       (_,computerZug) = berechneSpielstandUndZug groesse saat beginner zuege
       zuegeMitComputer = zuege ++ maybeToList computerZug
       (stand,_) =
          berechneSpielstandUndZug groesse saat beginner zuegeMitComputer
   in  Html.center (spielstand
          (groesse, saat, beginner, macheGegenzug, zuegeMitComputer) stand)

spiel :: String -> Html.Html
spiel s =
   case interpretiereAnfrage s of
      Just (groesse, saat, beginner, macheGegenzug, zuege) ->
         let spielbaum =
                GameTree.build moeglicheZuege
                   (grundstellung groesse saat beginner)
             spielaktuell =
                GameTree.selectDeepSubTree zuege spielbaum
             optZug = optimalerZug spielaktuell
             (spielunterbaum,zuegeMitComputer) =
                maybe
                   (spielaktuell, zuege)
                   (\zug -> (GameTree.selectSubTree zug spielaktuell, zuege++[zug]))
                (guard macheGegenzug >> optZug)
             stand = GameTree.state spielunterbaum
         in  spielstand
                (groesse, saat, beginner, macheGegenzug, zuegeMitComputer) stand
      Nothing ->
         toHtml $ "Mit dem Spielstand " ++ show s ++ " kann ich nichts anfangen."

start :: Int -> Html.Html
start saat =
   toHtml "Es beginnt" +++
   Html.simpleTable [] []
     (map (\orient ->
        map (\(gegenzug,spieler) ->
                labelAnchor
                    ("ZeilenSpalten?"++
                       erzeugeAnfrage ((10,10),saat,orient,gegenzug,[]))
                 << ("der "++spieler++" auf einer "++show orient++"."))
            [(False,"Mensch"),(True,"Computer")])
      [Spalte,Zeile]) Html.! [Html.border 2]


komplett :: Html.Html -> Html.Html
komplett body =
   Html.header (Html.thetitle << "Zeilen und Spalten") +++
   Html.body body +++
   Html.br +++ labelAnchor "ZeilenSpalten" << "Nochmal von vorne!"

-- Maybe String wird gebraucht um zwischen "?" und "" zu unterscheiden
erzeuge :: Maybe String -> IO Html.Html
erzeuge =
   maybe (fmap start randomIO) (return . spiel)


erzeugeAnfrage :: Beschreibung -> String
erzeugeAnfrage ((breite,hoehe),saat,orient,gegenzug,zuege) =
   CGI.formEncode $
      ("breite", show breite) :
      ("hoehe", show hoehe) :
      ("saat", show saat) :
      ("orient", show orient) :
      ("gegenzug", show gegenzug) :
      (if null zuege then [] else [("zuege", unwords $ map show zuege)]) ++
      []

interpretiereAnfrage :: String -> Maybe Beschreibung
interpretiereAnfrage anfrage =
   let paare = CGI.formDecode anfrage
   in  do breite   <- maybeRead =<< List.lookup "breite" paare
          hoehe    <- maybeRead =<< List.lookup "hoehe" paare
          saat     <- maybeRead =<< List.lookup "saat" paare
          orient   <- maybeRead =<< List.lookup "orient" paare
          gegenzug <- maybeRead =<< List.lookup "gegenzug" paare
          zuege <-
             case List.lookup "zuege" paare of
                Nothing -> Just []
                Just zuegeText ->
                   mapM (\zugText ->
                      case zugText of
                         _:_:_:_ -> Nothing
                         _ -> maybeRead zugText) $
                   words zuegeText
          return ((breite,hoehe), saat, orient, gegenzug, zuege)


main :: IO ()
main =
   putStr . Html.renderHtml . komplett =<< erzeuge . nullToMaybe =<< getLine
