{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Hakyll.Convert.Blogger as Blogger
import Hakyll.Convert.IO
import Hakyll.Convert.OutputFormat
import qualified Hakyll.Convert.Wordpress as Wordpress
import System.Console.CmdArgs
import System.Environment
import System.FilePath

data InputFormat = Blogger | Wordpress
  deriving (Data, Typeable, Enum, Show)

data Config = Config
  { feed :: FilePath,
    outputDir :: FilePath,
    -- underscore will be turned into a dash when generating a commandline
    -- option
    output_format :: T.Text,
    format :: InputFormat,
    extract_comments :: Bool
  }
  deriving (Show, Data, Typeable)

parameters :: FilePath -> Config
parameters p =
  modes
    [ Config
        { feed = def &= argPos 0 &= typ "ATOM/RSS FILE",
          outputDir = def &= argPos 1 &= typDir,
          output_format = "%o" &= help outputFormatHelp,
          format = Blogger &= help "blogger or wordpress",
          extract_comments = False &= help "Extract comments"
        }
        &= help "Save blog posts Blogger feed into individual posts"
    ]
    &= program (takeFileName p)

outputFormatHelp :: String
outputFormatHelp =
  unlines
    [ "Output filenames format (without extension)",
      "Default: %o",
      "Available formats:",
      "  %% - literal percent sign",
      "  %o - original filename (e.g. 2016/01/02/blog-post)",
      "  %s - original slug (e.g. \"blog-post\")",
      "  %y - publication year, 2 digits",
      "  %Y - publication year, 4 digits",
      "  %m - publication month",
      "  %d - publication day",
      "  %H - publication hour",
      "  %M - publication minute",
      "  %S - publication second"
    ]

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

main :: IO ()
main = do
  p <- getProgName
  config <- cmdArgs (parameters p)

  let ofmt = output_format config

  if not (T.null ofmt || validOutputFormat ofmt)
    then fail $ "Invalid output format string: `" ++ T.unpack (output_format config) ++ "'"
    else case format config of
      Blogger -> mainBlogger config
      Wordpress -> mainWordPress config

mainBlogger :: Config -> IO ()
mainBlogger config = do
  mfeed <- Blogger.readPosts (feed config)
  case mfeed of
    Nothing -> fail $ "Could not understand Atom feed: " ++ feed config
    Just fd -> mapM_ process fd
  where
    process fd = do
      let distilled = Blogger.distill (extract_comments config) fd
      fname <- savePost (outputDir config) (output_format config) "html" distilled
      putStrLn fname

mainWordPress :: Config -> IO ()
mainWordPress config = do
  mfeed <- Wordpress.readPosts (feed config)
  case mfeed of
    Nothing -> fail $ "Could not understand RSS feed: " ++ feed config
    Just fd -> mapM_ process fd
  where
    process fd = do
      let distilled = Wordpress.distill (extract_comments config) fd
      fname <- savePost (outputDir config) (output_format config) "markdown" distilled
      putStrLn fname
