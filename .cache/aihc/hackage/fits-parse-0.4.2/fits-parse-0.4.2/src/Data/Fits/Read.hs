{-# LANGUAGE OverloadedStrings #-}
module Data.Fits.Read where

import Control.Exception ( displayException )
import Data.Bifunctor ( first )
import Data.ByteString ( ByteString )
import Data.Maybe ( listToMaybe )
import Data.Text ( Text, unpack )
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Text.Megaparsec as M
import Data.List ( find )
import Lens.Micro ((^.))

---- local imports
import Data.Fits as Fits
import Data.Fits.MegaParser (ParseErr(..), parseHDU, parseHDUs)
import Data.Fits (HeaderDataUnit(..))


-- | Parse and read all HDUs in the input string
readHDUs :: ByteString -> Either String [HeaderDataUnit]
readHDUs bs = do
    first (show . ParseError) $ M.runParser parseHDUs "FITS" bs

-- | Parse and read only the Primary HDU from the input string
readPrimaryHDU :: ByteString -> Either String HeaderDataUnit
readPrimaryHDU bs = do
    first (show . ParseError) $ M.runParser parseHDU "FITS" bs

-- | Look up a keyword and parse it into the expected format
getKeyword :: Text -> (Value -> Maybe a) -> HeaderDataUnit -> Either String a
getKeyword k fromVal hdu = do
    v <- maybeError (MissingKey k) $ Fits.lookup k (hdu ^. header)
    maybeError (InvalidKey k v) $ fromVal v

-- | Get the HDU at an index and fail with a readable error
getHDU :: String -> Int -> [HeaderDataUnit] -> Either String HeaderDataUnit
getHDU name n hdus = do
    maybeError (MissingHDU name n) $ listToMaybe $ drop n hdus

maybeError :: FitsError -> Maybe a -> Either String a
maybeError e Nothing = Left (show e)
maybeError _ (Just a) = Right a

eitherFail :: MonadFail m => Either String a -> m a 
eitherFail (Left e) = fail e
eitherFail (Right a) = return a

data FitsError
    = ParseError ParseErr
    | MissingKey Text
    | InvalidKey Text Value
    | MissingHDU String Int 
    | InvalidData String
    deriving (Eq)

instance Show FitsError where
    show (ParseError e) = displayException e
    show (MissingKey k) = "Keyword Missing: " <> unpack k
    show (InvalidKey k val) = "Keyword: " <> unpack k <> " was invalid. Got " <> show val
    show (MissingHDU name n) = "HDU Missing: " <> name <> " at index " <> show n
    show (InvalidData err) = "Data Invalid: " <> err

-- -- | An example of how to use the library
-- example :: IO ()
-- example = do
--     bs <- BS.readFile  "./fits_files/nso_dkist.fits"
--
--     (tel, obs, dm) <- throwLeft $ exampleReadMyData bs
--
--     putStrLn $ "TELESCOPE: " <> unpack tel
--     putStrLn $ "OBSERVATORY: " <> unpack obs
--     putStrLn $ "DATAMIN: " <> show dm
--
--   where
--     throwLeft :: Show e => Either e a -> IO a
--     throwLeft (Left e) = fail $ show e
--     throwLeft (Right a) = return a
--
--     -- You can parse the file and lookup relevant data in the same function
--     exampleReadMyData :: ByteString -> Either String (Text, Text, Float)
--     exampleReadMyData bs = do
--       hdus <- readHDUs bs
--       hdu <- getHDU "Main Binary Table" 1 hdus
--       tel <- getKeyword "TELESCOP" toText hdu
--       obs <- getKeyword "OBSRVTRY" toText hdu
--       dm <- getKeyword "DATAMIN" toFloat hdu
--       return (tel, obs, dm)
--
