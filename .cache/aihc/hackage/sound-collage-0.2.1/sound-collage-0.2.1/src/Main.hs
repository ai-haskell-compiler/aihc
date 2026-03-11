module Main where

import qualified SoundCollage
import qualified Option
import qualified Options.Applicative as OP

import qualified Sound.SoxLib as SoxLib

import qualified Shell.Utility.Log as Shell

import qualified PathFormat as PathFmt
import qualified System.IO.Temp as Temp
import qualified System.Path as Path

import Control.Monad (liftM2, )
import Control.Applicative (pure, (<*>), )
import Data.Monoid ((<>), )
import Data.Maybe (isNothing, )


withSystemTempDirectory :: String -> (Path.AbsDir -> IO a) -> IO a
withSystemTempDirectory name f =
   Temp.withSystemTempDirectory name (f . Path.absDir)

setChunkSize :: Int -> SoundCollage.Parameters -> SoundCollage.Parameters
setChunkSize chunkSize params =
   params {
      SoundCollage.paramShift =
         div chunkSize (SoundCollage.paramOverlap params)
   }

setNumChannels :: Int -> SoundCollage.Parameters -> SoundCollage.Parameters
setNumChannels numChannels params =
   params {
      SoundCollage.paramChannels = numChannels
   }

setParams ::
   Maybe Int -> Maybe Int ->
   SoundCollage.Parameters -> SoundCollage.Parameters
setParams maybeChunkSize maybeChannels =
   maybe id setNumChannels maybeChannels .
   maybe id setChunkSize maybeChunkSize

makeParams :: Option.T -> SoundCollage.Parameters
makeParams (Option.Cons _verbosity maybeChunkSize maybeChannels) =
   setParams maybeChunkSize maybeChannels SoundCollage.defltParams

commandParser :: Option.Command (Option.T -> IO ())
commandParser =
   Option.decompose
      (pure (\flags -> SoundCollage.runDecompose (makeParams flags)))
   <>
   Option.decomposeSlow
      (pure (\flags -> SoundCollage.runDecomposeSlow (makeParams flags)))
   <>
   Option.compose
      (pure (\flags -> SoundCollage.runCompose (makeParams flags)))
   <>
   Option.associate
      (pure (\pool flags -> SoundCollage.runAssociate (makeParams flags) pool)
       <*> Option.pool)
   <>
   Option.adjacent
      (pure
         (\cohesion pool flags ->
            SoundCollage.runAdjacent (Option.verbosity flags)
               (makeParams flags) cohesion pool)
       <*> Option.cohesion
       <*> Option.pool)
   <>
   Option.auto
      (pure
         (\cohesion pool
               (Option.Cons verbosity maybeChunkSize maybeChannels) src dst ->
            withSystemTempDirectory "sound-chunks" $ \chunkDir ->
            withSystemTempDirectory "sound-collage" $ \collDir -> do
               params <-
                  fmap (setParams maybeChunkSize maybeChannels) $
                  if isNothing $ liftM2 (,) maybeChunkSize maybeChannels
                    then do
                       (chunkSize, numChannels) <-
                          SoundCollage.parametersFromPool pool
                       return $
                          setNumChannels numChannels $
                          setChunkSize chunkSize SoundCollage.defltParams
                    else return SoundCollage.defltParams
               Shell.notice verbosity $
                  "chunk size: " ++
                  show (SoundCollage.paramChunkSize params)
               Shell.notice verbosity $
                  "number of channels: " ++
                  show (SoundCollage.paramChannels params)
               Shell.notice verbosity $ "decompose to " ++ Path.toString chunkDir
               SoundCollage.runDecompose params src
                  (PathFmt.File chunkDir "%06d")
               Shell.notice verbosity $ "associate to " ++ Path.toString collDir
               SoundCollage.runAssociate params
                  pool chunkDir (PathFmt.File collDir "%06d")
               Shell.notice verbosity $ "update adjacent"
               SoundCollage.runAdjacent verbosity params cohesion
                  pool chunkDir collDir
               Shell.notice verbosity $ "compose"
               SoundCollage.runCompose params collDir dst)
       <*> Option.cohesion
       <*> Option.pool)

main :: IO ()
main = SoxLib.formatWith $ do
   action <-
      OP.execParser $
      OP.info
         (OP.helper <*> (OP.subparser commandParser <*> Option.parseFlags))
         Option.desc
   action
