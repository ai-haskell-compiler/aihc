{- | Ein- und Ausgabe fuer Game.VierGewinnt getrennt von Zugberechnung -}
module Game.VierGewinnt.HTML (
   komplett,
   erzeuge,
   main,
   ) where

import Game.VierGewinnt
   (Spieler(..), Zug, Spielstand, grundstellung, brettVon, wertung,
    anfangundzuege, moeglicheZuege, berechneSpielstand, istMatt, )
import Game.Utility (nullToMaybe)

import qualified Text.Html as Html
import qualified Data.List as List
import Text.Html((<<), (+++), noHtml, spaceHtml, concatHtml, renderHtml, toHtml)
import Text.Read.HT (maybeRead)
import Data.Array((!))

import qualified Network.CGI as CGI


farbe :: Maybe Spieler -> String
farbe Nothing         = Html.white
farbe (Just Mensch)   = Html.blue
farbe (Just Computer) = Html.red

labelAnchor :: String -> Html.Html -> Html.Html
labelAnchor ref label =
   Html.anchor label Html.! [Html.href ref]

relHeight, relWidth :: Int -> Html.HtmlAttr
relHeight r = Html.strAttr "HEIGHT" (show r ++ "%")
relWidth  r = Html.strAttr "WIDTH"  (show r ++ "%")

spielstand :: Spielstand -> Html.Html
spielstand stand = concatHtml [moeglichesEnde, table]
    where
    brett  = brettVon stand
    fmtf x = Html.td spaceHtml
                Html.! [relHeight 14, Html.bgcolor (farbe x)]

    table = Html.center (Html.table (concatHtml tableContents)
                Html.! [relWidth 65, relHeight 65, Html.border 2])
    tableContents =
        zuege:
        [Html.tr (concatHtml [fmtf (brett!(i,7-j)) | i<-[1..7]]) |
        j<-[1..6]]

    moeglichesEnde =
       case wertung stand of
          6000    -> Html.h2 << "Du bist matt!"
          (-6000) -> Html.h2 << "Du hast gewonnen!"
          _       -> noHtml

    zuege = Html.tr (concatHtml (map gencell [1..7]))

    {-
    fmt [] = "<h2>Unentschieden. Nochmal?</h2>"
    fmt x  = show x
    -}

    (anfang,altezuege)= anfangundzuege stand
    color :: Zug -> String
    color i =
        if null altezuege || i /= last altezuege
          then Html.white
          else Html.yellow

    gencell :: Zug -> Html.Html
    gencell i = Html.td (Html.center (href (toHtml ("Spalte " ++ show i))))
                   Html.! [Html.bgcolor (color i)]
        where
        href s =
            if elem i (moeglicheZuege stand) && not (istMatt stand)
              then Html.bold (labelAnchor ("VierGewinnt?" ++ erzeugeAnfrage anfang (altezuege++[i])) s)
              else s


komplett :: Html.Html -> Html.Html
komplett body =
   Html.header (Html.thetitle << "Vier gewinnt") +++
   Html.body
    (Html.h1 ("Vier-Gewinnt in " +++
                 labelAnchor "http://www.haskell.org/" << "Haskell" +++
              " (" +++ labelAnchor "VierGewinnt.hs" << "Quelltext" +++ ")") +++
              body +++
     labelAnchor "VierGewinnt" (Html.h2 << "Neues Spiel?") +++
     Html.hr +++
     labelAnchor "http://www.mathematik.uni-halle.de/~wensch" << "JW" +++
     ", " +++
     labelAnchor "http://www.mathematik.uni-halle.de/~podhaisky" << "HP")


-- Maybe String wird gebraucht um zwischen "?" und "" zu unterscheiden
erzeuge :: Maybe String -> Html.Html
erzeuge =
   maybe
     (concatHtml
        [Html.h2 << "Wer f\228ngt an?",
         (labelAnchor ("VierGewinnt?" ++ erzeugeAnfrage Computer []) << "Der Computer"),
         Html.h2 << "Nein, ich. Ich nehme:",
         spielstand (grundstellung Mensch)])
     (\s ->
        case interpretiereAnfrage s of
           Just stand -> spielstand (berechneSpielstand stand)
           Nothing -> toHtml $ "Mit dem Spielstand " ++ show s ++ " kann ich nichts anfangen.")

erzeugeAnfrage :: Spieler -> [Zug] -> String
erzeugeAnfrage spieler zuege =
   CGI.formEncode $
      ("start", show spieler) :
      (if null zuege then [] else [("zuege", unwords $ map show zuege)]) ++
      []

interpretiereAnfrage :: String -> Maybe (Spieler, [Zug])
interpretiereAnfrage anfrage =
   let paare = CGI.formDecode anfrage
   in  do anfangText <- List.lookup "start" paare
          anfang <- maybeRead anfangText
          zuege <-
             case List.lookup "zuege" paare of
                Nothing -> Just []
                Just zuegeText ->
                   mapM (\zugText ->
                      case zugText of
                         [_] -> maybeRead zugText
                         _ -> Nothing) $
                   words zuegeText
          return (anfang, zuege)


main :: IO ()
main =
   putStr . renderHtml . komplett . erzeuge . nullToMaybe =<< getLine
