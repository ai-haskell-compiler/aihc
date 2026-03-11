module System.Path.Internal.System where

import qualified Control.Monad.Trans.State as MS
import Control.Applicative ((<$>))

import Data.Tagged (Tagged)

import Test.QuickCheck (Gen)



class System os where
    -- | The character that separates directories. In the case where more than
    --   one character is possible, 'pathSeparator' is the \'ideal\' one.
    --
    -- >> Posix.isPathSeparator Posix.pathSeparator
    pathSeparator :: Tagged os Char

    -- | The list of all possible separators.
    --
    -- >> Posix.pathSeparator `elem` Posix.pathSeparators
    pathSeparators :: Tagged os [Char]
    pathSeparators = (:[]) <$> pathSeparator

    -- | Rather than using @(== 'pathSeparator')@, use this. Test if something
    --   is a path separator.
    --
    -- >> Posix.isPathSeparator a == (a `elem` Posix.pathSeparators)
    isPathSeparator :: Tagged os (Char -> Bool)
    isPathSeparator = flip elem <$> pathSeparators

    splitAbsolute :: Tagged os (MS.State String String)

    canonicalize :: Tagged os (String -> String)

    splitDrive :: Tagged os (MS.State String String)
    genDrive :: Tagged os (Gen String)
