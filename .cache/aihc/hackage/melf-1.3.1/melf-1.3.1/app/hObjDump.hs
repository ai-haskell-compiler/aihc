{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

import qualified Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Prettyprinter
import Prettyprinter.Util
import Options.Applicative

import Data.Elf
import Data.Elf.PrettyPrint

data Options = Options
    { optArgs :: [String]
    , optFull :: Bool
    }

opts' :: Parser Options
opts' = do
  optFull <- switch (  short 'f'
                    <> long "full"
                    <> help "Don't shorten data and symbol tables"
                    )
  optArgs <- some $ argument str (  metavar "FILE" )
  pure Options {..}

opts :: ParserInfo Options
opts = info (opts' <**> helper)
  (  fullDesc
  <> progDesc "Dump ELF files FILEs"
  <> header "hobjdump - dump ELF files"
  )

printFile :: Bool -> String -> IO ()
printFile full fileName = do
    bs <- fromStrict <$> BS.readFile fileName
    elf <- parseElf bs
    doc <- printElf_ full elf
    putDocW 80 (doc  <> line)

main :: IO ()
main = do
    Options {..} <- execParser opts
    mapM_ (printFile optFull) optArgs
