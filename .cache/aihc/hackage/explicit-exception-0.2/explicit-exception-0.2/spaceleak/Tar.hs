module Main where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEntry
import Control.Monad.Exception.Asynchronous
   (Exceptional(Exceptional), force, pure, throwMonoid, result, exception, )
import qualified Data.ByteString.Lazy as B
import Prelude hiding (pure)


convert ::
   Tar.Entries Tar.FormatError -> Exceptional Tar.FormatError [Tar.Entry]
convert =
   force .
   Tar.foldEntries
      (\entry -> fmap (entry:))
      (pure [])
      throwMonoid

-- the String argument prevents caching and thus a space-leak
infinite :: String -> Tar.Entries e
infinite name =
   let tar =
          Tar.Next
             (TarEntry.directoryEntry $ either error id $
              TarEntry.toTarPath True name)
             tar
   in  tar

test :: String
test =
   map (const 'a') $ result $ convert $ infinite "test"

spaceLeak0 :: IO ()
spaceLeak0 =
   let r  = convert $ infinite "bla"
       e  = exception r
       xs = result r
   in  do mapM_ print [ "dir" | Tar.NormalFile _ _ <- map Tar.entryContent xs ]
          print e

spaceLeak1 :: IO ()
spaceLeak1 =
   let Exceptional e xs = convert $ infinite "bla"
   in  do mapM_ print [ "dir" | Tar.NormalFile _ _ <- map Tar.entryContent xs ]
          print e

{-
tar c /data1/ | ghc +RTS -M32m -c30 -RTS -e spaceLeak src/Tar.hs

tar c /data1/ | ./dist/build/ee-tar/ee-tar +RTS -M32m -c30 -RTS
-}
spaceLeak :: IO ()
spaceLeak = do
   tar <- B.getContents
   let a  = convert (Tar.read tar)
--       e  = exception a
       xs = result a
   print [ B.length bs | Tar.NormalFile bs _ <- map Tar.entryContent xs ]
--   print e

tarFold :: IO ()
tarFold = do
   tar <- B.getContents
   Tar.foldEntries
      (\x rest ->
          case Tar.entryContent x of
             Tar.NormalFile bs _ -> print (B.length bs) >> rest
             _ -> rest)
      (return ())
      print
      (Tar.read tar)

main :: IO ()
main = spaceLeak1
