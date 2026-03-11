{-# LANGUAGE LambdaCase #-}

module Internal.Text.Read where

import Control.Monad
    ( replicateM_ )
import Text.Read
    ( ReadPrec, get, look, pfail )

import qualified Data.List as L

readCharMaybe :: (Char -> Maybe a) -> ReadPrec a
readCharMaybe f = look >>= \case
    a : _ | Just c <- f a ->
        get >> pure c
    _ ->
        pfail

skipChar :: Char -> ReadPrec ()
skipChar charToSkip = readCharMaybe
    (\char -> if char == charToSkip then Just () else Nothing)

skipString :: String -> ReadPrec ()
skipString stringToSkip = do
    remainder <- look
    if stringToSkip `L.isPrefixOf` remainder
    then replicateM_ (length stringToSkip) get
    else pfail
