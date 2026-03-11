{-# LANGUAGE
      CPP,
      OverloadedStrings,
      StrictData
  #-}

-- | Intended to be used as a Haskell preprocessor, so call it with
-- options including:
--
-- > ghc -F -pgmF tophat -optF <any extra options>
--
-- This process is documented [by GHC](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#ghc-flag--F).

module Main where

-- liftA2 is in Prelude from GHC9.6 onwards
#if MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
#else
import Control.Applicative (liftA2)
#endif
import Control.Monad ((<=<))
import Data.Char (isSpace)
import qualified Data.List as L
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getArgs, getProgName)
import System.FilePath.Posix ((</>))

import HaskellLex (splitCodeOn)
import Include (Inclusion(..), readTemplate)
import Report
import Text (linebreaks, splits, splitOnOnce)


{-
-- Not used: I'm scared of breaking Haskell's indentation conventions.

display :: (Show a, IsString s) => a -> s
display = fromString . show

linePragma :: (Semigroup s, IsString s) => String -> Int -> s
linePragma name number =
  "\n{-# LINE " <>
  display number <>
  " " <>
  display name <>
  " #-}\n"
-}


data Arguments = Arguments {
  progName :: String,
  originalFilename :: FilePath,
  inFilename :: FilePath,
  outFilename :: FilePath,
  includer :: FilePath -> Inclusion
}


usageNote :: String -> Text
usageNote prog =
  "Usage: " <>
  T.pack prog <>
  T.unlines [" <original> <input> <output> <other args>",
             "See https://hackage.haskell.org/tophat/",
             "",
             "Arguments:",
             "  templates=<dir>   set template directory",
             "  opener=<delim>    set opening delimiter",
             "  closer=<delim>    set closing delimiter",
             "  start-code        start templates in code",
             "  start-literal     start templates in literal text",
             "  end-code          check that templates end in code",
             "  end-literal       check that templates end in literals",
             "  end-either        allow templates to end in code or literals",
             "  check-balance     check that code fragments have balanced parentheses"]


makeArgs :: String -> [String] -> E Arguments
makeArgs prog (originalFile:inFile:outFile:extraArgs) = modifyArgs =<< traverse (parseArg . T.pack) extraArgs where

  defaultArgs = Arguments {
    progName = prog,
    originalFilename = originalFile,
    inFilename = inFile,
    outFilename = outFile,
    includer = \ n -> Inclusion {
      filename = n,
      openCode = "$",
      closeCode = "$",
      startLiteral = True,
      mayEndLiteral = True,
      mayEndCode = False,
      checkCodeBalanced = True
    }
  }

  parseArg :: Text -> E (Text, Maybe Text)
  parseArg t = pure $ case splitOnOnce "=" t of
    Nothing -> (t, Nothing)
    Just (a,b) -> (a, Just b)

  modifyArgs :: [(Text, Maybe Text)] -> E Arguments
  modifyArgs a = do
    f <- configure (actGlobalArg <$> a)
    pure $ f defaultArgs

makeArgs prog _ = failure $ usageNote prog


actGlobalArg :: (Text, Maybe Text) -> E (Arguments -> Arguments)
actGlobalArg t@(k,v) = case k of
  "templates" -> case v of
    Nothing -> failure "templates takes an argument"
    Just d  -> pure argDirectory where
      argDirectory a = a { includer = \ n -> incDirectory (includer a n) n}
      incDirectory i n = i { filename = T.unpack d </> n }
  _ -> boost <$> actInclusionArg t where
    boost :: (Inclusion -> Inclusion) -> (Arguments -> Arguments)
    boost f a = a { includer = f . includer a }

actInclusionArg :: (Text, Maybe Text) -> E (Inclusion -> Inclusion)
actInclusionArg (k,v) = case k of
  "opener" -> case v of
    Just d  -> pure (\ i -> i { openCode = d })
    Nothing -> failure "opener takes an argument"
  "closer" -> case v of
    Just d  -> pure (\ i -> i { closeCode = d })
    Nothing -> failure "closer takes an argument"  
  "start-code" -> case v of
    Just _ -> failure "start-code doesn't take an argument"
    _      -> pure (\ i -> i { startLiteral = False })
  "start-literal" -> case v of
    Just _ -> failure "start-literal doesn't take an argument"
    _      -> pure (\ i -> i { startLiteral = True })
  "end-literal" -> case v of
    Just _ -> failure "end-literal doesn't take an argument"
    _      -> pure (\ i -> i { mayEndLiteral = True, mayEndCode = False })
  "end-code" -> case v of
    Just _ -> failure "end-code doesn't take an argument"
    _      -> pure (\ i -> i { mayEndLiteral = False, mayEndCode = True })
  "end-either" -> case v of
    Just _ -> failure "end-code doesn't take an argument"
    _      -> pure (\ i -> i { mayEndLiteral = True, mayEndCode = True })
  "check-balance" -> case v of
    Just _ -> failure "check-balance doesn't take an argument"
    _      -> pure (\ i -> i { checkCodeBalanced = True })
  "no-check-balance" -> case v of
    Just _ -> failure "no-check-balance doesn't take an argument"
    _      -> pure (\ i -> i { checkCodeBalanced = False })
  _ -> failure ("Unrecognised argument: " <> k)

modifyInclusion :: [(Text,Maybe Text)] -> E (Inclusion -> Inclusion)
modifyInclusion extras = configure (actInclusionArg <$> extras)


preprocess :: Arguments -> I ()
preprocess args = do
  input <- liftIO . T.readFile $ inFilename args
  output <- assemble =<< liftE (parseCode 1 input)
  liftIO $ T.writeFile (outFilename args) output where

    -- This is, perhaps, a good place for some parallelism; we could
    -- hope to read multiple templates in parallel.
    assemble :: [Either Inclusion Builder] -> I Text
    assemble = fmap (T.toLazyText . mconcat) . traverse f where
      f (Left i) = readTemplate i
      f (Right t) = pure t

    parseCode :: Int -> Text -> E [Either Inclusion Builder]
    parseCode _ "" = pure []
    parseCode n t = case splitCodeOn "{{" t of
      Nothing    -> pure [Right (snd $ processCode n t)]
      Just (c,r) -> let (n', b) = processCode n c
                    in (Right b:) <$> parseTemplate n' r

    processCode :: Int -> Text -> (Int, Builder)
    processCode n t =
      mconcat . fmap T.fromLazyText . L.intersperse "\n" <$> advance n (T.splitOn "\n" t) where
        f _ = id -- formerly "(linePragma (originalFilename args) i <>)"
        advance i [] = (i,[])
        advance i (c:cs) = let
          (j,cs') = advance (i+1) cs
          in (j,f i c:cs')

    parseTemplate :: Int -> Text -> E [Either Inclusion Builder]
    parseTemplate startLine = inner . splitOnOnce "}}" where
      inner Nothing = failure "Unterminated template"
      inner (Just (templateText, remainderText)) = liftA2 (:) template remainder where
        remainder = parseCode (startLine+linebreaks templateText) remainderText
        template = Left <$> makeInclusion templateText

        makeInclusion :: Text -> E Inclusion
        makeInclusion = act <=< separate <=< parse where

          parse :: Text -> E [(Text, Maybe Text)]
          parse = getKey where
            
            getKey t = f $ splits t where
              f [] = pure [(process t, Nothing)]
              f [('\\',_)] = failure "Inclusion ends in '\\'"
              f (('\\',_):_:l) = f l
              f (('=',(k,r)):_) = getOpt (process k) r
              f ((',',(k,r)):_) = ((process k, Nothing):) <$> getKey r
              f (_:l) = f l
                
            getOpt k t = f $ splits t where
              f [] = pure [(k, Just $ process t)]
              f [('\\',_)] = failure "Inclusion ends in '\\'"
              f (('\\',_):_:l) = f l
              f ((',',(v,r)):_) = ((k, Just $ process v):) <$> getKey r
              f (_:l) = f l

            -- Remove initial whitespace, trailing unescaped
            -- whitespace, and replace escaped characters
            process :: Text -> Text
            process = T.pack . f . splits . T.stripStart where
              f [] = []
              f [('\\',_)] = error "This shouldn't have happened"
              f (('\\',_):(c,_):l) = c:f l
              f ((c,(_,r)):l)
                | isSpace c && T.null (T.strip r) = []
                | otherwise                       = c:f l

          separate :: [(Text, Maybe Text)] -> E ([(Text,Maybe Text)], Text)
          separate [] = failure "There's no filename"
          separate ((_,Just _):_) = failure "The filename is not supposed to be given a value"
          separate ((f,Nothing):l) = pure (l,f)

          act :: ([(Text,Maybe Text)], Text) -> E Inclusion
          act (extras, name) = do
            f <- modifyInclusion extras
            let i = includer args $ T.unpack name
            pure $ f i


main :: IO ()
main = doOrDie $ do
  args <- liftIO getArgs
  prog <- liftIO getProgName
  config <- liftE $ makeArgs prog args
  preprocess config
