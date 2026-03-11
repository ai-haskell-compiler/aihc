module Parse where

import Test.DocTest.Parse (DocTest, parseComment)
import Test.DocTest.Location (Located(Located), unLoc)

import qualified ModuleName

import qualified System.Path as Path

import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.Reverse.StrictSpine as Rev
import qualified Data.List.HT as ListHT
import Data.Functor.Identity (Identity, runIdentity)
import Data.Traversable (traverse)
import Data.NonEmpty ((!:))
import Data.Maybe.HT (toMaybe)
import Data.Maybe (fromMaybe, isNothing)
import Data.Tuple.HT (mapFst)
import Data.Char (isSpace)

import qualified Control.Monad.Trans.State as MS
import qualified Control.Functor.HT as FuncHT
import Control.Monad (guard)
import Control.Applicative (optional, (<$>), (<|>))



type Line = Int
type Column = Int

data Module a =
   Module {
      moduleName :: ModuleName.T,
      modulePath :: Path.AbsRelFile,
      modulePragma :: Maybe (Located Line [String]),
      moduleSetup :: Maybe (Located Line [Located Column String]),
      moduleContent :: [a]
   } deriving (Show)

emptyModule :: Module a -> Bool
emptyModule (Module _ _ pragma setup tests) =
   null tests && isNothing pragma && isNothing setup


moduleFromLines ::
   ModuleName.T -> Path.AbsRelFile ->
   [Located Line String] -> Module [Located (Line, Column) String]
moduleFromLines modName path nLines =
   let (pragmas,setup) =
         FuncHT.unzip $ fmap extractPragmas $ extractInit "setup" nLines
   in Module {
         moduleName = modName,
         modulePath = path,
         modulePragma = pragmas,
         moduleSetup = setup,
         moduleContent = extractComments nLines
      }


type FlexParser = MS.StateT (Located Column String)
type LineParser = FlexParser Maybe

parseLine :: (Monad m) => FlexParser m a -> String -> m a
parseLine p line = MS.evalStateT p $ Located 0 line

parseWithLineNumber ::
   (Monad m) =>
   FlexParser m () -> Located n String -> m (Located (n,Column) String)
parseWithLineNumber p (Located n line) =
   parseLine (p >> getWithLineNumber n) line

getWithLineNumber ::
   (Monad m) => n -> FlexParser m (Located (n,Column) String)
getWithLineNumber n = MS.gets $ \(Located col line) -> Located (n,col) line

nextChar :: LineParser Char
nextChar = MS.StateT $ \(Located col line0) -> do
   (c,line1) <- ListHT.viewL line0
   return (c, Located (col+1) line1)

matchChar :: Char -> LineParser ()
matchChar c = guard . (c==) =<< nextChar

match :: String -> LineParser ()
match = mapM_ matchChar

matchEnd :: LineParser ()
matchEnd = MS.gets (null . unLoc) >>= guard

{-
For @m = Maybe@ it holds

> trap p = p <|> return ()
-}
trap :: (Monad m) => LineParser () -> FlexParser m ()
trap p = MS.modify $ \line -> fromMaybe line $ MS.execStateT p line

class (Monad m) => ToMaybe m where monadToMaybe :: m a -> Maybe a
instance ToMaybe Maybe where monadToMaybe = id
instance ToMaybe Identity where monadToMaybe = Just . runIdentity

skip :: (ToMaybe m) => (Char -> Bool) -> FlexParser m ()
skip cond =
   let go = trap (nextChar >>= guard . cond >> MS.mapStateT monadToMaybe go)
   in go

skipSpaces :: (ToMaybe m) => FlexParser m ()
skipSpaces = skip isSpace


matchCommentHead ::
   String -> Located n String ->
   Maybe ([Located n String] ->
          (NonEmpty.T [] (Located (n, Column) String), [Located n String]))
matchCommentHead ident (Located n line) =
   let matchIdent = match ident >> skipSpaces >> getWithLineNumber n
   in flip parseLine line $ do
         skipSpaces
         singleLineComments <$> (match "-- " >> matchIdent)
            <|>
            multiLineComment <$>
               (match "{-" >> optional (matchChar ' ') >> matchIdent)

extractPragmas ::
   Located Line [Located col String] ->
   (Located Line [String], Located Line [Located col String])
extractPragmas (Located start initial) =
   let (pragmas,setup) =
         ListHT.spanJust
            (\(Located _ ln) ->
               ListHT.maybePrefixOf ":set " ln
               <|>
               toMaybe (null $ dropWhile isSpace ln) "")
            initial
   in (Located start pragmas, Located (start + length pragmas) setup)

extractInit ::
   String -> [Located n String] -> Maybe (Located n [Located Column String])
extractInit name =
   fmap (\fsuff ->
      let NonEmpty.Cons (Located (n,col) ln) lns = fst $ uncurry ($) fsuff
      in Located n $
         ((Located col $
           if null ln then "" else "-- junk after setup keyword: " ++ ln) : ) $
         ListHT.takeWhileJust $
         map
            (parseLine (skipSpaces >> match ">>>" >>
                        (matchChar ' ' <|> matchEnd) >> MS.get) . unLoc)
            lns)
   .
   ListHT.dropWhileNothing (matchCommentHead ('$':name))

extractComments :: [Located n String] -> [[Located (n, Column) String]]
extractComments =
   let go ts =
         case ListHT.dropWhileNothing (matchCommentHead "|") ts of
            Nothing -> []
            Just (splitPart,suffix) ->
               let (part,remainder) = splitPart suffix
               in NonEmpty.flatten part : go remainder
   in go

singleLineComments ::
   Located (n, Column) String -> [Located n String] ->
   (NonEmpty.T [] (Located (n, Column) String), [Located n String])
singleLineComments firstLine suffix =
   mapFst (firstLine!:) $
   ListHT.spanJust
      (parseWithLineNumber $ skipSpaces >> match "--" >> skipSpaces)
      suffix

multiLineComment ::
   Located (n, Column) String -> [Located n String] ->
   (NonEmpty.T [] (Located (n, Column) String), [Located n String])
multiLineComment firstLine0 suffix0 =
   let maybeClosing = ListHT.maybeSuffixOf "-}" . Rev.dropWhile isSpace
   in case traverse maybeClosing firstLine0 of
         Just firstLine1 -> (NonEmpty.singleton firstLine1, suffix0)
         Nothing ->
            let (part,msuffix1) =
                  ListHT.breakJust (traverse maybeClosing) suffix0
                unindent = runIdentity . parseWithLineNumber skipSpaces
            in mapFst (NonEmpty.appendRight (firstLine0 !: map unindent part)) $
               case msuffix1 of
                  Nothing -> ([], [])
                  Just (lastLine,suffix1) -> ([unindent lastLine], suffix1)

numberedLines :: String -> [Located Line String]
numberedLines = zipWith Located [1..] . lines


reindentModule ::
   Module [Located (line,Column) String] ->
   Module [Located (line,Column) String]
reindentModule modu =
   modu {moduleContent =
            flip map (moduleContent modu) $
               zipWith ($)
                  (id :
                     repeat
                        (\(Located (line,col) str) ->
                              Located (line,0) $ replicate col ' ' ++ str))}

-- | Convert documentation to 'Example's.
parseModule :: Module [Located pos String] -> Module [Located pos DocTest]
parseModule modu =
   modu {moduleContent =
            filter (not . null) $ map parseComment $ moduleContent modu}
