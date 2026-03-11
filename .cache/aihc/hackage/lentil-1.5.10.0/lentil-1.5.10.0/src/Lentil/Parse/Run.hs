-----------------------------------------------------------------------------
-- |
-- Module      :  Lentil.Parse.Run
-- Copyright   :  © 2015 Francesco Ariis
-- License     :  GPLv3 (see the LICENSE file)
--
-- Parsing functions interface
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Lentil.Parse.Run where

import Lentil.Types
import Lentil.Helpers
import Lentil.Parse.Issue
import Lentil.Parse.Source
import Lentil.Parse.Syntaxes

import Text.Megaparsec

import qualified Control.DeepSeq      as DS
import qualified Control.Monad        as CM
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Trans  as T
import qualified Data.ByteString      as B
import qualified Data.DList           as D
import qualified Data.IORef           as I
import qualified Data.Maybe           as M
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Text.Lazy       as TL
import qualified System.Directory     as D
import qualified System.ProgressBar   as PB

-----------
-- TYPES --
-----------

data RunEnv = RunEnv
            { -- reader part
              reAliases :: [Alias],    -- for parsing purposes
              reFWords  :: [FlagWord], -- for parsing purposes

              -- stateful part
              reFiles   :: I.IORef [FilePath],      -- fnames to process
              reIssues  :: I.IORef (D.DList Issue), -- processed issues
              reBar     :: PB.ProgressBar () }      -- progress bar

newtype Runner a = Runner { runRunner :: R.ReaderT RunEnv IO a }
            deriving (Functor, Applicative, Monad,
                      T.MonadIO, R.MonadReader RunEnv)


issueFinder :: [Alias] -> [FlagWord] -> [FilePath] -> IO [Issue]
issueFinder as fws fps =
            creaEnv >>= \e ->
            let r = runRunner runnerFinder in
            R.runReaderT r e
    where
          creaEnv :: IO RunEnv
          creaEnv =
                let bIO = initProgressBar (length fps) in
                I.newIORef fps     >>= \ifs ->
                I.newIORef D.empty >>= \iis ->
                R.liftIO bIO       >>= \pb ->
                return (RunEnv as fws
                               ifs iis pb)


runnerFinder :: Runner [Issue]
runnerFinder = looperino                    >>
               D.toList <$> asksIO reIssues
    where
          looperino :: Runner ()
          looperino =
                asksIO reFiles >>= \case
                  [] -> return ()
                  _  -> processFile >>
                        looperino

processFile :: Runner ()
processFile = -- 1. does exist
              fetchFile        >>= \fp ->
              doesFileExist fp >>= \bf ->
              if not bf
                then return ()
              else

              -- 2. execute parse (and store results)
              parseFile fp >>

              -- 3. update counter and blit it
              blitCounter


-------------
-- PARSING --
-------------

parseFile :: FilePath -> Runner ()
parseFile fp =
        -- 1. language parser finder
        R.asks reAliases >>= \as ->
        case langParserAlias as fp of
          Nothing -> return ()
          Just p  -> -- 2. parse comments
                     parseComments fp p >>= \cs ->
                     -- 3. parse issues (and write them)
                     mapM_ (parseIssues fp) cs

parseComments :: FilePath -> StateParser () [CommentString] ->
                 Runner [Comment]
parseComments fp p =
            T.liftIO (safeRead fp) >>= \t ->
            runPar (comms2Tuple <$> p) fp () t
            -- meglio di
            -- comms2Tuple <$> runPar p fp () t
            -- così evitiamo istanza deepseq per CommentString

-- from a single comment
parseIssues :: FilePath -> (Row, String) -> Runner ()
parseIssues fp (r, cs) =
            -- parse
            R.asks reFWords         >>= \fws ->
            runPar issPar fp fws cs >>= \dis ->

            -- write
            let appd bd = D.append bd dis      in
            R.asks reIssues                    >>= \iis ->
            T.liftIO (I.modifyIORef' iis appd)
    where
          issPar :: StateParser [FlagWord] (D.DList Issue)
          issPar = D.fromList <$> (setRow r >> issues)

-- generic parsing
runPar :: (Monoid o, DS.NFData o) =>
          StateParser s o -> FilePath -> s -> String -> Runner o
runPar p fp s i =
        case runStateParser p s fp i of
          Left l  -> rperr (fp ++ " : parse error " ++
                            errorBundlePretty l)       >>
                     return mempty
          Right r -> return (DS.deepseq r r)

-- todo [refactor] Row should be carried on by issues, not be manually set!
setRow :: Row -> ParIssue ()
setRow r = updateParserState
             (\(State i o (PosState pix po (SourcePos n _ cx) tw lp) es) ->
                let l' = mkPos r in
               State i o (PosState pix po (SourcePos n l' cx) tw lp) es)


------------------
-- PROGRESS BAR --
------------------

blitCounter :: Runner ()
blitCounter = R.asks reBar                   >>= \pb ->
              T.liftIO (PB.incProgress pb 1)

-- t: total issues
initProgressBar :: Int -> IO (PB.ProgressBar ())
initProgressBar t = PB.newProgressBar myBar 2 (PB.Progress 0 t ())
    where
          myBar = PB.defStyle {
                    PB.styleDone = '*',
                    PB.styleCurrent = '*',
                    PB.styleTodo = ' ',
                    PB.styleWidth = PB.ConstantWidth 40,
                    PB.stylePrefix = PB.msg barMes
                    }

          barMes = TL.pack $ show t ++ " source files"


-----------------
-- ANCILLARIES --
-----------------

asksIO :: (RunEnv -> I.IORef a) -> Runner a
asksIO a = R.asks a                >>= \ia ->
           T.liftIO (I.readIORef ia)

fetchFile :: Runner FilePath
fetchFile = asksIO reFiles >>= \case
              []     -> error "(assert) fetchFile called on empty filelist!"
              (f:fs) -> R.asks reFiles               >>= \ifs ->
                        T.liftIO (I.writeIORef ifs fs) >>
                        return f

doesFileExist :: FilePath -> Runner Bool
doesFileExist fp =
            T.liftIO (D.doesFileExist fp)             >>= \fb ->
            CM.unless fb
                    (rperr $ fp ++ " : no such file") >>
            return fb

rperr :: String -> Runner ()
rperr s = T.liftIO $ perr s

-- todo Do not use String, but Text. [u:1]
-- todo Add a proper testing interface [u:2]


-- Try system locale, then utf-8 it if fails, then latin-1 as a last ditch.
safeRead :: FilePath -> IO String
safeRead fp = do
        -- We are *not* relying on system locale here, just
        -- trying a number of popular encodings for source files.
        b <- B.readFile fp
        case decodeMulti decoders b of
          Nothing -> perr (fp ++ ": unrecognised encoding.") >>
                     return ""
          Just t -> return (T.unpack t)
    where
        decodeMulti :: [B.ByteString -> Maybe T.Text] ->
                       B.ByteString -> Maybe T.Text
        decodeMulti wds b =
          case M.mapMaybe ($ b) wds of
            [] -> Nothing
            (t:_) -> Just t

        decoders :: [B.ByteString -> Maybe T.Text]
        decoders =
          [(\b -> either (const Nothing) Just (TE.decodeUtf8' b)), -- utf8.
           (\b -> Just (TE.decodeLatin1 b))] -- latin-1 (for Windows).
