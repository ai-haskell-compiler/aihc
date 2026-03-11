{- | Input and output for Game.Mastermind -}
module Game.Mastermind.HTML (
   complete,
   generate,
   main,
   ) where

import qualified Game.Mastermind.CodeSet.Tree as CodeSetTree
import qualified Game.Mastermind.CodeSet as CodeSet
import qualified Game.Mastermind.NonEmptyEnumSet as NonEmptySet
import qualified Game.Mastermind as MM
import Game.Utility (nullToMaybe, randomSelect)

import qualified Text.Html as Html
import Text.Html((<<), (+++), concatHtml, toHtml)
import Text.Read.HT (maybeRead)

import qualified Network.CGI as CGI

import qualified Data.List as List
import qualified Data.List.HT as ListHT
import qualified Data.NonEmpty as NonEmpty
import Data.Foldable (fold, foldMap)
import Data.NonEmpty ((!:))
import Data.Tuple.HT (mapSnd)
import Data.Maybe.HT (toMaybe, )

import qualified Control.Monad.Trans.State as MS
import Control.Monad (liftM2, replicateM, )

import qualified System.Random as Rnd


labelAnchor :: String -> Html.Html -> Html.Html
labelAnchor ref label = Html.anchor label Html.! [Html.href ref]

concatMapHtml :: (a -> Html.Html) -> [a] -> Html.Html
concatMapHtml f = concatHtml . map f

maybeSingle :: (a -> b) -> Maybe a -> [b]
maybeSingle f = foldMap (\a -> [f a])


type Move = (String, MM.Eval)
type Config = (Int, NonEmpty.T [] Char, Int, Maybe [Move], Maybe String)

evaluation :: MM.Eval -> Html.Html
evaluation (MM.Eval rightPlaces rightSymbols) =
   (Html.table $
      Html.tr $
      concatMapHtml
          (\color -> (Html.td << Html.spaceHtml) Html.! [Html.bgcolor color]) $
         replicate rightPlaces Html.black ++
         replicate rightSymbols Html.white)
      Html.! [Html.border 2]

state ::
   Config ->
   Maybe (CodeSetTree.T Char) ->
   Maybe String ->
   Html.Html
state (width, alphabet, seed, mMoves, mAttempt) mRemaining mCheck =
   let moves = fold mMoves
       select name options =
          (Html.select $ concatMapHtml (Html.option <<) options)
          Html.! [Html.name name]
       verify code eval =
          case mCheck of
             Nothing -> []
             Just check ->
                let shouldBeEval = MM.evaluate check code
                in if shouldBeEval == eval
                      then [toHtml " "]
                      else [toHtml "sollte sein: ", evaluation shouldBeEval]
       won =
          not (null moves) &&
          case last moves of
             (_, MM.Eval rightPlaces _) -> rightPlaces == width
       codeTds = map ((Html.! [Html.align "center"]) . Html.td . toHtml)
   in  Html.center $
       (Html.! [Html.action "Mastermind"]) $
       Html.form $ concatHtml $
          [Html.hidden "width" (show width),
           Html.hidden "alphabet" (NonEmpty.flatten alphabet),
           Html.hidden "seed" (show seed),
           Html.hidden "moves" (unwords $ map formatMove moves)]
          ++
          maybeSingle (Html.hidden "attempt") mAttempt
          ++
          [(Html.table $
            concatMapHtml Html.tr $
            zipWith
                 (\n row -> (Html.th << (show n ++ ".")) +++ row)
                 [(0::Int)..] $
              flip map moves (\(code, eval) ->
                 concatHtml $
                 codeTds code ++
                 map Html.td (evaluation eval : verify code eval))
              ++
              if won || maybe False CodeSet.null mRemaining
                then []
                else [
                   maybe
                      (Html.td (Html.textfield "attempt" Html.!
                          [Html.maxlength width]) Html.! [Html.colspan width]
                       +++
                       Html.td (Html.submit "" "abschicken"))
                      (\attempt ->
                          concatHtml $
                          codeTds attempt ++
                          let numbers = map show [0..width]
                          in [Html.td $ Html.simpleTable [] []
                                [[evaluation (MM.Eval 1 0),
                                  select "rightplaces" numbers,
                                  Html.spaceHtml,
                                  evaluation (MM.Eval 0 1),
                                  select "rightsymbols" numbers,
                                  Html.spaceHtml,
                                  Html.submit "" "bewerten"]]])
                      mAttempt])
              -- Html.! [Html.border 2]
           ]
           ++
           (case mRemaining of
               Nothing -> []
               Just remaining ->
                  [case CodeSet.size remaining of
                     0 ->
                        toHtml "Die Bewertungen sind widerspr\252chlich."
                        +++
                        Html.br
                        +++
                        toHtml "Welchen Code meinten Sie? "
                        +++
                        Html.textfield "check" Html.! [Html.maxlength width]
                        +++
                        Html.submit "" "pr\252fen"

                     1 -> toHtml "Dies ist die einzige verbleibende M\246glichkeit."
                     n ->
                        toHtml
                           ("Es bleiben noch " ++ show n ++
                            " M\246glichkeiten. Hier eine kleine Auswahl:")
                        +++
                        (Html.ordList $ take 10 $ CodeSet.flatten remaining)])
           ++
           (if won
              then [Html.br, Html.bold << "R\228tsel gel\246st!"]
              else [])


game :: String -> Html.Html
game s =
   case parseQuery s of
      Just ((width, symbols, seed, mMoves, mAttempt), mCheck) ->
         case (mMoves,mAttempt) of
            (Just moves, Nothing) ->
               let alphabet = NonEmptySet.fromList symbols
                   remaining =
                      CodeSet.compress $
                      CodeSet.intersections $
                      CodeSet.cube alphabet width !:
                      map (uncurry (MM.matching
                              (NonEmptySet.flatten alphabet))) moves
                   (attempt,newSeed) =
                      mapSnd (fst . Rnd.random) $
                      MS.runState
                         (MM.scanningRandomizedAttempt width
                            (NonEmptySet.flatten alphabet) moves remaining)
                         (Rnd.mkStdGen seed)
               in  state
                      (width, symbols, newSeed, Just moves, attempt)
                      (Just remaining)
                      mCheck
            _ ->
               let code =
                      MS.evalState
                         (replicateM width $ randomSelect $
                          NonEmpty.flatten symbols)
                         (Rnd.mkStdGen seed)
               in  state
                      (width, symbols, seed,
                       Just $
                          fold mMoves ++
                          maybeSingle
                             (\attempt -> (attempt, MM.evaluate code attempt))
                             mAttempt,
                       Nothing)
                      Nothing
                      mCheck
      Nothing ->
         toHtml $
            "Mit dem Spielstand " ++ show s ++ " kann ich nichts anfangen."

start :: Int -> Html.Html
start seed =
   toHtml "Es r\228t" +++
   Html.simpleTable [] []
      (ListHT.outerProduct
         (\(alphabet,typ,widthName) (computerAttempts,player) ->
            toHtml ("der "++player++" "++typ++" mit ")
            +++
            (concatHtml $ List.intersperse (toHtml ", ") $
               map
                  (\width ->
                   labelAnchor
                       ("Mastermind?"++
                          formatQuery
                             (width, alphabet, seed,
                              toMaybe computerAttempts [], Nothing))
                    << show width)
                  [3..7])
            +++
            toHtml (" "++widthName++"."))
         [('0'!:['1'..'9'], "Zahlen", "Stellen"),
          ('a'!:['b'..'z'], "W\246rter", "Buchstaben")]
         [(False,"Mensch"),(True,"Computer")])
      Html.! [Html.border 2]


complete :: Html.Html -> Html.Html
complete body =
   Html.header (Html.thetitle << "Mastermind") +++
   Html.body body +++
   Html.br +++ labelAnchor "Mastermind" << "Noch einmal von vorne!"

-- need Maybe String in order to distinguish between "?" and ""
generate :: Maybe String -> IO Html.Html
generate = maybe (fmap start Rnd.randomIO) (return . game)


formatQuery :: Config -> String
formatQuery (width, alphabet, seed, mMoves, mAttempt) =
   CGI.formEncode $
      ("width", show width) :
      ("alphabet", NonEmpty.flatten alphabet) :
      ("seed", show seed) :
      (case mAttempt of
          Nothing -> []
          Just attempt -> [("attempt", attempt)]) ++
      (case mMoves of
          Nothing -> []
          Just moves -> [("moves", unwords $ map formatMove moves)]) ++
      []

formatMove :: (String, MM.Eval) -> String
formatMove (code, MM.Eval rightPlaces rightSymbols) =
   code ++ "-" ++ show rightPlaces ++ "-" ++ show rightSymbols

parseQuery :: String -> Maybe (Config, Maybe String)
parseQuery query =
   let pairs = CGI.formDecode query
   in  do width    <- maybeRead =<< List.lookup "width" pairs
          alphabet <- NonEmpty.fetch =<< List.lookup "alphabet" pairs
          seed     <- maybeRead =<< List.lookup "seed" pairs
          mMoves <-
             maybe (Just Nothing)
                (fmap Just .
                 mapM (\moveText ->
                   case ListHT.chop ('-' ==) moveText of
                      [code,rightPlacesText,rightSymbolsText] ->
                         fmap ((,) code) $
                         liftM2 MM.Eval
                            (maybeRead rightPlacesText)
                            (maybeRead rightSymbolsText)
                      _ -> Nothing)
                 .
                 words) $
             List.lookup "moves" pairs
          let mAttempt0 = List.lookup "attempt" pairs
              mRightPlaces = fmap maybeRead $ List.lookup "rightplaces" pairs
              mRightSymbols = fmap maybeRead $ List.lookup "rightsymbols" pairs
          (moves,mAttempt) <-
             case mMoves of
                Nothing -> Just (Nothing, Nothing)
                Just moves0 ->
                   case liftM2 (,) mAttempt0 $
                        liftM2 (,) mRightPlaces mRightSymbols of
                      Just (move, mEval) ->
                         fmap (\eval ->
                                  (Just $ moves0 ++ [(move,eval)], Nothing)) $
                         uncurry (liftM2 MM.Eval) mEval
                      Nothing -> Just (Just moves0, mAttempt0)
          return ((width, alphabet, seed, moves, mAttempt),
                  List.lookup "check" pairs)


main :: IO ()
main =
   putStr . Html.renderHtml . complete =<< generate . nullToMaybe =<< getLine
