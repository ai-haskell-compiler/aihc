{-# LANGUAGE CPP             #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Monad

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                    ((<>))
#endif

import qualified Data.ByteString.Char8          as C
import           Data.ByteString.Internal.Ascii
import           Options.Applicative
import           Prelude                        hiding (length)
import           System.Exit
import           System.Random.MWC

import           Data.NanoID
import           Options

main :: IO ()
main = do
  Options{..} <- execParser opts
  if showver
    then putStrLn showVer >> exitFailure
    else if length < 1 || length > 21
      then strFail "nanoid length"
    else if quantity < 1
      then strFail "quantity"
      else do
        let mAlphabet =
              if | password                               -> Just specialPassword
                 | alphabet == unAlphabet defaultAlphabet -> Just defaultAlphabet
                 | isAscii alphabet                       -> Just (Alphabet alphabet)
                 | otherwise                              -> Nothing
        case mAlphabet of
          Just a -> do
            g <- createSystemRandom
            replicateM_ quantity $
              customNanoID a (toEnum length) g >>= putNanoID newline
            exitSuccess
          Nothing -> strFail "alphabet that is not made of ascii chars only"
  where
    strFail m = putStrLn ("Bad " <> m <> ". See help (-h).") >> exitFailure
    putNanoID nl = put nl . unNanoID
      where put nl' = if nl' then C.putStrLn else C.putStr

