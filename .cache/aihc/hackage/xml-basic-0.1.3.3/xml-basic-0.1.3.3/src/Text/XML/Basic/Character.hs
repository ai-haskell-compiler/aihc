{-|
All kinds of representations of a character in XML combined in one type.
Note that an entity can in principle represent a large text,
thus an \"XML character\" might actually be a text.
However the standard entities consist of one character.
In contrast to our representation,
HaXml uses Unicode substrings instead of Unicode characters,
which is certainly more efficient for common XML texts
that contain mainly Unicode text and only few references.
However our representation is unique,
whereas HaXmls may represent a text as @"abc","def"@ or @"abcdef"@.
-}
module Text.XML.Basic.Character (
   T(..), toUnicode, toUnicodeGen,
   toUnicodeOrFormat, toUnicodeOrFormatGen,
   fromUnicode, fromCharRef, fromEntityRef,
   maybeUnicode, maybeCharRef, maybeEntityRef,
   isUnicode, isCharRef, isEntityRef, isRef,
   unicode, refC, refE,
   asciiFromUnicode, asciiFromUnicodeGen, minimalRefFromUnicode,
   reduceRef, reduceRefGen,
   validCharRef, switchUnicodeRuns,
   ) where

import qualified Text.XML.Basic.Format as Fmt
import qualified Text.XML.Basic.Entity as Ent
import qualified Data.Map as Map
import qualified Data.Char as Char
import Data.Maybe.HT (toMaybe, )
import Data.Tuple.HT (mapFst, )
import Control.Monad (mplus, )

import qualified Control.Monad.Exception.Synchronous as Exc


data T =
     Unicode Char
   | CharRef Int
   | EntityRef String
      deriving (Eq)

{- |
If a reference cannot be resolved
then an @Exception@ constructor with an error message is returned.
-}
toUnicode :: T -> Exc.Exceptional String Char
toUnicode =
   toUnicodeGen Ent.mapNameToChar

toUnicodeGen :: Map.Map String Char -> T -> Exc.Exceptional String Char
toUnicodeGen _ (Unicode c) = Exc.Success c
toUnicodeGen _ (CharRef c) =
   if validCharRef c
     then Exc.Success $ Char.chr c
     else Exc.Exception $ "Character number out of bound: " ++ show c
toUnicodeGen dict (EntityRef name) =
   maybe (Exc.Exception $ "Unknown entity &" ++ name ++ ";") Exc.Success $
   Map.lookup name dict


{- |
If a reference cannot be resolved
then a reference string is returned.
-}
toUnicodeOrFormat :: T -> ShowS
toUnicodeOrFormat =
   toUnicodeOrFormatGen Ent.mapNameToChar

toUnicodeOrFormatGen :: Map.Map String Char -> T -> ShowS
toUnicodeOrFormatGen dict =
   Fmt.run . reduceRefGen dict


fromUnicode :: Char -> T
fromUnicode = Unicode

fromCharRef :: Int -> T
fromCharRef = CharRef

fromEntityRef :: String -> T
fromEntityRef = EntityRef


maybeUnicode :: T -> Maybe Char
maybeUnicode (Unicode c) = Just c
maybeUnicode _           = Nothing

maybeCharRef :: T -> Maybe Int
maybeCharRef (CharRef n) = Just n
maybeCharRef _           = Nothing

maybeEntityRef :: T -> Maybe String
maybeEntityRef (EntityRef s) = Just s
maybeEntityRef _             = Nothing


isUnicode :: T -> Bool
isUnicode (Unicode _) = True
isUnicode _           = False

isCharRef :: T -> Bool
isCharRef (CharRef _) = True
isCharRef _           = False

isEntityRef :: T -> Bool
isEntityRef (EntityRef _) = True
isEntityRef _             = False

isRef :: T -> Bool
isRef x = isCharRef x && isEntityRef x



{-|
Convert unicode character to XML Char,
where Unicode constructor is only used for ASCII characters.
This is achieved by the following decision:
If there is a entity reference, use this.
If it is ASCII, represent it as Char.
Otherwise use a character reference.
-}
asciiFromUnicode :: Char -> T
asciiFromUnicode =
   asciiFromUnicodeGen Ent.mapCharToName

asciiFromUnicodeGen :: Map.Map Char String -> Char -> T
asciiFromUnicodeGen dict c =
   maybe
      (if Char.isAscii c
         then fromUnicode c
         else fromCharRef (Char.ord c))
      fromEntityRef $
   Map.lookup c dict


{- |
Generate XML character from Unicode character
with minimal use of references.
The only references used are the XML entity references
@&apos;@, @&quot;@, @&amp;@, @&lt;@, @&gt;@.
-}
minimalRefFromUnicode :: Char -> T
minimalRefFromUnicode c =
   maybe
      (fromUnicode c)
      fromEntityRef $
   Map.lookup c Ent.mapCharToName


-- * shortcuts for making the output of the Show instance valid

unicode :: Char -> T
unicode = Unicode

refC :: Int -> T
refC = fromCharRef

refE :: String -> T
refE = fromEntityRef


switchUnicodeRuns ::
   (String -> a) -> (Int -> a) -> (String -> a) ->
   [T] -> [a]
switchUnicodeRuns uni charRef entRef =
   let prepend (Unicode c) rest =
          mapFst (Left . (c:)) $
          case rest of
             (Left s : ss) -> (s, ss)
             _ -> ([], rest)
       prepend (CharRef   n) rest = (Right (charRef n), rest)
       prepend (EntityRef n) rest = (Right (entRef  n), rest)
   in  map (either uni id) .
       foldr (\c -> uncurry (:) . prepend c) []


instance Show T where
   showsPrec prec a =
      showParen (prec >= 10) $
      case a of
         Unicode   c -> showString "unicode " . shows c
         CharRef   n -> showString "refC " . shows n
         EntityRef n -> showString "refE " . shows n
   showList =
      showParen True .
      foldr (.) (showString "[]") .
      switchUnicodeRuns
         (\str -> showString "map unicode " . shows str . showString " ++ ")
         (\n -> showString "refC " . shows n . showString  " : ")
         (\n -> showString "refE " . shows n . showString  " : ")


instance Fmt.C T where
   run (Unicode c) = showChar c
   run (CharRef n) = Fmt.amp . Fmt.sharp . shows n . Fmt.semicolon
   run (EntityRef n) = Fmt.amp . showString n . Fmt.semicolon



{- |
Reduce the use of references.
Represent as much as possible characters as Unicode characters,
that is, using the Unicode constructor.
-}
reduceRef :: T -> T
reduceRef = reduceRefGen Ent.mapNameToChar

{- | try to convert a References to equivalent Unicode characters -}
reduceRefGen :: Map.Map String Char -> T -> T
reduceRefGen dict x =
   maybe x Unicode $
   mplus
      (flip Map.lookup dict =<< maybeEntityRef x)
      (do n <- maybeCharRef x
          toMaybe (validCharRef n) (Char.chr n))

validCharRef :: Int -> Bool
validCharRef n =
   0 <= n && n <= Char.ord maxBound
