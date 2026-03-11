module ShakeRules where

import Development.Shake
import Development.Shake.FilePath
import Control.Monad (guard, when)
import Text.Printf (printf)
import qualified Data.List as List



data Configuration =
    Configuration {
        useBackground :: Bool,
        frameRate :: Integer,
        resolution :: Integer,
        antialias :: [String],
        pages :: [String]
    }

defaultConfig :: Configuration
defaultConfig =
    Configuration {
        useBackground = True,
        frameRate = 50,
        resolution = 108,
        antialias = ["-dTextAlphaBits=2", "-dGraphicsAlphaBits=2"],
        pages = []
    }

previewConfig :: Configuration
previewConfig =
    Configuration {
        useBackground = False,
        frameRate = 25,
        resolution = 54,
        antialias = [],
        pages = []
    }

tmpRoot :: FilePath
tmpRoot = "/tmp"

tmpFramesDir :: String -> FilePath
tmpFramesDir stem = tmpRoot </> stem <> "-frames"


{-
tif24nc and ppmraw are much faster than png16m,
but occupy more temporary space.
-}
framePattern :: FilePath; frameFormat :: String
(framePattern, frameFormat) =
    case id '_' of
        'p' -> ("%04d.png", "png16m")
        't' -> ("%04d.tif", "tif24nc")
        _   -> ("%04d.ppm", "ppmraw")

require :: FilePath -> Action FilePath
require path = do
    need [path]
    return path

whenAvailable :: FilePath -> (FilePath -> Action ()) -> Action ()
whenAvailable path act = do
    available <- doesFileExist path
    when available $ act path


chooseAudioSource :: FilePath -> [String] -> Action FilePath
chooseAudioSource stem audioFormats = do
    let audioSrcs = map (stem <.>) audioFormats
    available <- traverse doesFileExist audioSrcs
    case filter fst $ zip available audioSrcs of
        (_,audioSrc):_ -> return audioSrc
        _ -> fail "No audio source available"


configured :: Configuration -> FilePath -> Rules ()
configured config prefix = do
    prefix <> "*.pdf" %> \dst -> do
        let base = takeBaseName dst
        midi <- require $ base <.> "mid"
        let background = base <.> "jpeg"
        backgroundAvailable <-
            if useBackground config
                then doesFileExist background
                else return False
        let optionsFile = base <.> "options"
        optionsAvailable <- doesFileExist optionsFile
        options <-
            if optionsAvailable
                then readFile' optionsFile
                else return ""
        let titleFile = base <.> "title"
        titleAvailable <- doesFileExist titleFile
        title <-
            if titleAvailable
                then fmap (\str -> ["--title", str]) $ readFile' titleFile
                else return []
        cmd_ "boomwhacker" title options "--rate" (show $ frameRate config)
                (guard backgroundAvailable >> ["--background", background])
                [midi, dst]

    tmpRoot </> prefix <> "*.frames" %> \dst -> do
        let stem =
                maybe
                    (error "cannot strip temporary directory prefix")
                    (dropExtension . dropWhile (pathSeparator==))
                    (List.stripPrefix tmpRoot dst)
        pdf <- require $ stem <.> "pdf"
        let framesDir = tmpFramesDir stem
        cmd_ "mkdir" "-p" [framesDir] -- avoid failure of 'rm -r'
        cmd_ "rm" "-r" [framesDir]
        cmd_ "mkdir" "-p" [framesDir]
        cmd_ "gs-parallel" "-j6"
          (pages config) pdf (framesDir </> framePattern) "--"
          "-dNOPAUSE" "-dBATCH"
          ("-sDEVICE="++frameFormat) (antialias config)
          [printf "-dDEVICEHEIGHT=%d" (resolution config * 10) :: String]
          [printf "-r%d" (resolution config) :: String]
        cmd_ "touch" [dst]

    prefix <> "*.flv" %> \dst -> do
        let stem = dropExtension dst
        need [tmpRoot </> stem <.> "frames"]
        let framesDir = tmpFramesDir stem
        audioSrc <- chooseAudioSource (takeBaseName dst) ["mp3", "aac", "wav"]
        cmd_ "ffmpeg" "-r" (show $ frameRate config) "-f" "image2"
            "-i" [framesDir </> framePattern]
            "-i" [audioSrc]
            "-vcodec" "flashsv" "-acodec" "copy" "-y" [dst]

    prefix <> "*.mp4" %> \dst -> do
        let stem = dropExtension dst
        need [tmpRoot </> stem <.> "frames"]
        let framesDir = tmpFramesDir stem
        audioSrc <-
            chooseAudioSource (takeBaseName dst) ["mp3", "aac", "flac", "wav"]
        cmd_ "ffmpeg" "-r" (show $ frameRate config) "-f" "image2"
            "-i" [framesDir </> framePattern]
            "-i" [audioSrc]
            "-b:v" "8000k" "-acodec" "copy" "-y" [dst]


generic :: Rules ()
generic = do
    "//*.inspect-csv" %> \dst -> do
        let Just stem = stripExtension "inspect-csv" dst
        src <- require $ stem <.> "mid"
        cmd_ "midicsv" [src, dst]

    "//*.mid" %> \dst ->
        whenAvailable (dst -<.> "csv") $ \src -> do
            cmd_ "csvmidi" [src, dst]

    "//*.csv" %> \dst ->
        whenAvailable (dst -<.> "fods") $ \src -> do
            Stdout csvUtf8 <- cmd "ods2csv" "--sheetnumber" "1" [src]
            cmd_ "iconv" "-f" "utf8" "-t" "latin1"
                    (StdinBS csvUtf8) (FileStdout dst)

    "//*.wav" %> \dst ->
        whenAvailable (dst -<.> "mid") $ \src -> do
            cmd_ "timidity" "--preserve-silence" "-A300" "-Ow" [src]
