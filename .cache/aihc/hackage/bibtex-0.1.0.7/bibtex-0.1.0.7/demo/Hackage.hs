module Main where

import qualified Text.BibTeX.Format as Format
import qualified Text.BibTeX.Entry as Entry
import qualified Text.LaTeX.Character as LaTeX

import qualified Distribution.PackageDescription.Parsec as PkgP
import qualified Distribution.PackageDescription as PkgD
import qualified Distribution.Package as Pkg
import qualified Distribution.Types.PackageName as PkgN
import qualified Distribution.Verbosity as Verbosity
import Distribution.PackageDescription
                   (PackageDescription, )
import Distribution.Package
                   (PackageIdentifier(..), )
import Distribution.PackageDescription.Parsec
                   (parseGenericPackageDescription, )
import Distribution.Simple.PackageDescription
                   (readGenericPackageDescription, )
import System.Time (ClockTime(TOD), getClockTime,
                    toCalendarTime, toUTCTime,
                    CalendarTime, ctYear, ctMonth, )

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEnt
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as B
import qualified System.IO as IO

import Distribution.Text (display, )
import Distribution.Pretty (prettyShow, )
import Distribution.Utils.ShortText (fromShortText, )

import Data.String.HT (trim, )
import Data.Tuple.HT  (mapFst, )
import Data.List.HT   (switchL, switchR, )
import Data.Char      (toLower, isSpace, isAlpha, chr, )
import qualified Data.List as List


{- |
See hackage-server:Distribution/Server/Pages/Package.hs
-}
packageURL :: PackageIdentifier -> String
packageURL pkgid = "/package/" ++ display pkgid


{- |
Filter out parts in parentheses and e-mails addresses
-}
removeAnnotations :: String -> String
removeAnnotations "" = ""
removeAnnotations (c:cs) =
   case c of
      '(' -> removeAnnotations $ drop 1 $ dropWhile (')'/=) cs
      '<' -> removeAnnotations $ drop 1 $ dropWhile ('>'/=) cs
      _ -> c : removeAnnotations cs

splitList :: String -> [String]
splitList =
   let separate rest = ([], uncurry (:) $ recourse rest)
       continue c rest = mapFst (c:) $ recourse rest
       recourse str =
          case str of
             '/' : rest -> separate rest
             '&' : rest -> separate rest
             ',' : rest -> separate rest
             c0:rest0@('a':'n':'d':c1:rest) ->
               if isSpace c0 && isSpace c1
                 then separate rest
                 else continue c0 rest0
             c:rest -> continue c rest
             "" -> ([], [])
   in  uncurry (:) . recourse

splitAuthorList :: String -> [String]
splitAuthorList =
   map (\author ->
      case author of
         "..." -> "others"
         "et al." -> "others"
         _ -> author) .
   filter (not . null) .
   map trim .
   splitList .
   -- remove numbers, quotation marks ...
   filter (\c -> isAlpha c || isSpace c || elem c ".-'@/&,") .
   removeAnnotations

{- authors must be split with respect to ',', '/', '&' and ' and ' -}
fromPackage :: CalendarTime -> PackageDescription -> Entry.T
fromPackage time pkg =
   let authors =
          splitAuthorList $ fromShortText $ PkgD.author pkg
       surname =
          switchL "unknown" (\firstAuthor _ ->
             switchR "" (\_ -> filter isAlpha) $
             words firstAuthor) $
          authors
       pkgId = PkgD.package pkg
       name = PkgN.unPackageName (Pkg.pkgName pkgId)
       year = ctYear time
       versionStr = prettyShow (Pkg.pkgVersion pkgId)
       bibId =
          map toLower surname ++ show year ++
          name ++ "-" ++ versionStr
   in  Entry.Cons "Misc" bibId $
       ("author",
           if null authors
             then "unknown"
             else Format.authorList $
                  map LaTeX.fromUnicodeString authors) :
       ("title",
           "{" ++ name ++ ": " ++
           LaTeX.fromUnicodeString (fromShortText (PkgD.synopsis pkg)) ++ "}") :
       ("howpublished",
           "\\url{http://hackage.haskell.org" ++
           packageURL (PkgD.package pkg) ++ "}") :
       ("year", show year) :
       ("month", show (ctMonth time)) :
       ("version", versionStr) :
       ("keywords", "Haskell, " ++ fromShortText (PkgD.category pkg)) :
       ("subtype", "program") :
       []

example :: IO ()
example =
   do now <- toCalendarTime =<< getClockTime
      pkg <- readGenericPackageDescription Verbosity.silent "example.cabal"
      putStrLn (Format.entry $ fromPackage now $ PkgD.packageDescription pkg)


{- |
This decodes UTF-8 but in contrast to UTF8.toString
it handles invalid characters like Latin-1 ones.
This way we can also cope with many texts that contain actually Latin-1.
-}
decodeUTF8orLatin :: B.ByteString -> String
decodeUTF8orLatin =
   List.unfoldr (\bstr ->
      flip fmap (UTF8.uncons bstr) $ \(c, rest) ->
         if c==UTF8.replacement_char
           then (chr $ fromIntegral $ B.head bstr, B.tail bstr)
           else (c,rest))


fromTarEntry :: Tar.Entry -> String
fromTarEntry ent =
   case (List.isSuffixOf ".cabal" $ TarEnt.entryPath ent,
         TarEnt.entryContent ent) of
      (True, TarEnt.NormalFile txt _size) ->
         case PkgP.runParseResult
                  (parseGenericPackageDescription (B.toStrict txt)) of
            (_, Right pkg) ->
               Format.entry $
               fromPackage
                  (toUTCTime (TOD (fromIntegral $ TarEnt.entryTime ent) 0))
                  (PkgD.packageDescription pkg)
            (_, Left err) -> show err
      _ -> ""

main :: IO ()
main =
   Tar.foldEntries
      (\entry cont -> putStrLn (fromTarEntry entry) >> cont)
      (return ()) (IO.hPutStr IO.stderr . show) .
   Tar.read =<<
   B.getContents
