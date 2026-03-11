-- Do not edit! Automatically created with doctest-extract from src/System/Path/Internal.hs
{-# LINE 199 "src/System/Path/Internal.hs" #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Test.Posix.System.Path.Internal where

import qualified System.Path.Posix as Default
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 201 "src/System/Path/Internal.hs" #-}
import     qualified System.Path.PartClass as Class
import     qualified System.Path.Generic as Path
import     qualified System.Path.Posix as Posix
import     qualified System.Path.Windows as Windows
import     System.Path.Generic ((</>), (<.>), relFile, relDir, absFile, absDir)
import     Data.List (isSuffixOf, isPrefixOf)
import     Data.Char (toLower)
import     qualified Test.QuickCheck as QC
forAllAbsRel     :: (Class.FileDir fd, QC.Testable prop) => (Default.AbsRel fd -> prop) -> QC.Property
forAllAbsRel     = QC.forAll QC.arbitrary

test :: DocTest.T ()
test = do
 DocTest.printPrefix "System.Path.Internal:281: "
{-# LINE 281 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 281 "src/System/Path/Internal.hs" #-}
      Path.pathMap (map toLower) (absDir "/tmp/Reports/SpreadSheets") == Posix.absDir "/tmp/reports/spreadsheets"
  )
 DocTest.printPrefix "System.Path.Internal:354: "
{-# LINE 354 "src/System/Path/Internal.hs" #-}
 DocTest.example(
{-# LINE 354 "src/System/Path/Internal.hs" #-}
    Posix.rootDir </> relDir "bla" </> relFile "blub"
  )
  [ExpectedLine [LineChunk "rootDir </> relPath \"bla\" </> relPath \"blub\""]]
 DocTest.printPrefix "System.Path.Internal:356: "
{-# LINE 356 "src/System/Path/Internal.hs" #-}
 DocTest.example(
{-# LINE 356 "src/System/Path/Internal.hs" #-}
    Just (Posix.rootDir </> relDir "bla" </> relFile "blub")
  )
  [ExpectedLine [LineChunk "Just (rootDir </> relPath \"bla\" </> relPath \"blub\")"]]
 DocTest.printPrefix "System.Path.Internal:358: "
{-# LINE 358 "src/System/Path/Internal.hs" #-}
 DocTest.example(
{-# LINE 358 "src/System/Path/Internal.hs" #-}
    Posix.currentDir </> relDir "bla" </> relFile "blub"
  )
  [ExpectedLine [LineChunk "currentDir </> relPath \"bla\" </> relPath \"blub\""]]
 DocTest.printPrefix "System.Path.Internal:360: "
{-# LINE 360 "src/System/Path/Internal.hs" #-}
 DocTest.example(
{-# LINE 360 "src/System/Path/Internal.hs" #-}
    Just (Posix.currentDir </> relDir "bla" </> relFile "blub")
  )
  [ExpectedLine [LineChunk "Just (currentDir </> relPath \"bla\" </> relPath \"blub\")"]]
 DocTest.printPrefix "System.Path.Internal:362: "
{-# LINE 362 "src/System/Path/Internal.hs" #-}
 DocTest.example(
{-# LINE 362 "src/System/Path/Internal.hs" #-}
    Windows.absDir "c:" </> relDir "bla" </> relFile "blub"
  )
  [ExpectedLine [LineChunk "absDir \"c:\" </> relPath \"bla\" </> relPath \"blub\""]]
 DocTest.printPrefix "System.Path.Internal:364: "
{-# LINE 364 "src/System/Path/Internal.hs" #-}
 DocTest.example(
{-# LINE 364 "src/System/Path/Internal.hs" #-}
    Just (Windows.absDir "c:\\" </> relDir "bla" </> relFile "blub")
  )
  [ExpectedLine [LineChunk "Just (absDir \"c:\\\\\" </> relPath \"bla\" </> relPath \"blub\")"]]
 DocTest.printPrefix "System.Path.Internal:476: "
{-# LINE 476 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 476 "src/System/Path/Internal.hs" #-}
      \p -> Path.asPath (Path.toString p) == (p::Default.AbsFile)
  )
 DocTest.printPrefix "System.Path.Internal:558: "
{-# LINE 558 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 558 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/" :: Maybe Posix.AbsDir) == Just "/"
  )
 DocTest.printPrefix "System.Path.Internal:559: "
{-# LINE 559 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 559 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/" :: Maybe Posix.AbsFile) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:560: "
{-# LINE 560 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 560 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/" :: Maybe Posix.RelDir) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:561: "
{-# LINE 561 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 561 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/" :: Maybe Posix.RelFile) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:562: "
{-# LINE 562 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 562 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/tmp" :: Maybe Posix.AbsDir) == Just "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:563: "
{-# LINE 563 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 563 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/tmp" :: Maybe Posix.AbsFile) == Just "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:564: "
{-# LINE 564 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 564 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/tmp" :: Maybe Posix.RelDir) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:565: "
{-# LINE 565 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 565 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/tmp" :: Maybe Posix.RelFile) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:566: "
{-# LINE 566 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 566 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/tmp/" :: Maybe Posix.AbsDir) == Just "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:567: "
{-# LINE 567 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 567 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/tmp/" :: Maybe Posix.AbsFile) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:568: "
{-# LINE 568 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 568 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/tmp/" :: Maybe Posix.RelDir) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:569: "
{-# LINE 569 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 569 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/tmp/" :: Maybe Posix.RelFile) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:570: "
{-# LINE 570 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 570 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/tmp" :: Maybe Posix.AbsRelFileDir) == Just "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:571: "
{-# LINE 571 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 571 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "/tmp/" :: Maybe Posix.AbsRelFileDir) == Just "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:572: "
{-# LINE 572 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 572 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "file.txt" :: Maybe Posix.RelFile) == Just "file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:573: "
{-# LINE 573 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 573 "src/System/Path/Internal.hs" #-}
      fmap Posix.toString (Posix.maybePath "file.txt" :: Maybe Posix.AbsFile) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:574: "
{-# LINE 574 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 574 "src/System/Path/Internal.hs" #-}
      fmap Windows.toString (Windows.maybePath "\\tmp" :: Maybe Windows.AbsDir) == Just "\\tmp"
  )
 DocTest.printPrefix "System.Path.Internal:575: "
{-# LINE 575 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 575 "src/System/Path/Internal.hs" #-}
      fmap Windows.toString (Windows.maybePath "a:\\tmp" :: Maybe Windows.AbsDir) == Just "a:\\tmp"
  )
 DocTest.printPrefix "System.Path.Internal:576: "
{-# LINE 576 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 576 "src/System/Path/Internal.hs" #-}
      fmap Windows.toString (Windows.maybePath "a:tmp" :: Maybe Windows.AbsDir) == Just "a:tmp"
  )
 DocTest.printPrefix "System.Path.Internal:577: "
{-# LINE 577 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 577 "src/System/Path/Internal.hs" #-}
      fmap Windows.toString (Windows.maybePath "a:\\" :: Maybe Windows.AbsDir) == Just "a:\\"
  )
 DocTest.printPrefix "System.Path.Internal:578: "
{-# LINE 578 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 578 "src/System/Path/Internal.hs" #-}
      fmap Windows.toString (Windows.maybePath "a:" :: Maybe Windows.AbsDir) == Just "a:"
  )
 DocTest.printPrefix "System.Path.Internal:579: "
{-# LINE 579 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 579 "src/System/Path/Internal.hs" #-}
      fmap Windows.toString (Windows.maybePath "tmp" :: Maybe Windows.RelDir) == Just "tmp"
  )
 DocTest.printPrefix "System.Path.Internal:580: "
{-# LINE 580 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 580 "src/System/Path/Internal.hs" #-}
      fmap Windows.toString (Windows.maybePath "\\tmp" :: Maybe Windows.RelDir) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:581: "
{-# LINE 581 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 581 "src/System/Path/Internal.hs" #-}
      fmap Windows.toString (Windows.maybePath "a:\\tmp" :: Maybe Windows.RelDir) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:582: "
{-# LINE 582 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 582 "src/System/Path/Internal.hs" #-}
      fmap Windows.toString (Windows.maybePath "a:tmp" :: Maybe Windows.RelDir) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:583: "
{-# LINE 583 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 583 "src/System/Path/Internal.hs" #-}
      fmap Windows.toString (Windows.maybePath "tmp" :: Maybe Windows.AbsDir) == Nothing
  )
 DocTest.printPrefix "System.Path.Internal:632: "
{-# LINE 632 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 632 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.relFile "file.txt") == "file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:633: "
{-# LINE 633 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 633 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.relFile "tmp") == "tmp"
  )
 DocTest.printPrefix "System.Path.Internal:639: "
{-# LINE 639 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 639 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.relDir ".") == "."
  )
 DocTest.printPrefix "System.Path.Internal:640: "
{-# LINE 640 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 640 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.relDir "file.txt") == "file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:641: "
{-# LINE 641 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 641 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.relDir "tmp") == "tmp"
  )
 DocTest.printPrefix "System.Path.Internal:647: "
{-# LINE 647 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 647 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.absFile "/file.txt") == "/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:648: "
{-# LINE 648 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 648 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.absFile "/tmp") == "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:654: "
{-# LINE 654 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 654 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.absDir "/file.txt") == "/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:655: "
{-# LINE 655 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 655 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.absDir "/tmp") == "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:750: "
{-# LINE 750 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 750 "src/System/Path/Internal.hs" #-}
      Posix.asPath "/tmp" == Posix.absDir "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:751: "
{-# LINE 751 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 751 "src/System/Path/Internal.hs" #-}
      Posix.asPath "file.txt" == Posix.relFile "file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:752: "
{-# LINE 752 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 752 "src/System/Path/Internal.hs" #-}
      Path.isAbsolute (Posix.asAbsDir "/tmp")
  )
 DocTest.printPrefix "System.Path.Internal:753: "
{-# LINE 753 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 753 "src/System/Path/Internal.hs" #-}
      Path.isRelative (Posix.asRelDir "/tmp")
  )
 DocTest.printPrefix "System.Path.Internal:754: "
{-# LINE 754 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 754 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asPath "/tmp" :: Posix.AbsDir) == "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:755: "
{-# LINE 755 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 755 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asPath "/tmp" :: Posix.RelDir) == "tmp"
  )
 DocTest.printPrefix "System.Path.Internal:756: "
{-# LINE 756 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 756 "src/System/Path/Internal.hs" #-}
      Windows.toString (Windows.asPath "\\tmp" :: Windows.AbsDir) == "\\tmp"
  )
 DocTest.printPrefix "System.Path.Internal:757: "
{-# LINE 757 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 757 "src/System/Path/Internal.hs" #-}
      Windows.toString (Windows.asPath "a:\\tmp" :: Windows.AbsDir) == "a:\\tmp"
  )
 DocTest.printPrefix "System.Path.Internal:758: "
{-# LINE 758 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 758 "src/System/Path/Internal.hs" #-}
      Windows.toString (Windows.asPath "a:tmp" :: Windows.AbsDir) == "a:tmp"
  )
 DocTest.printPrefix "System.Path.Internal:759: "
{-# LINE 759 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 759 "src/System/Path/Internal.hs" #-}
      Windows.toString (Windows.asPath "tmp" :: Windows.RelDir) == "tmp"
  )
 DocTest.printPrefix "System.Path.Internal:767: "
{-# LINE 767 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 767 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asRelFile "file.txt") == "file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:768: "
{-# LINE 768 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 768 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asRelFile "/file.txt") == "file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:769: "
{-# LINE 769 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 769 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asRelFile "tmp") == "tmp"
  )
 DocTest.printPrefix "System.Path.Internal:770: "
{-# LINE 770 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 770 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asRelFile "/tmp") == "tmp"
  )
 DocTest.printPrefix "System.Path.Internal:776: "
{-# LINE 776 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 776 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asRelDir ".") == "."
  )
 DocTest.printPrefix "System.Path.Internal:777: "
{-# LINE 777 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 777 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asRelDir "file.txt") == "file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:778: "
{-# LINE 778 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 778 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asRelDir "/file.txt") == "file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:779: "
{-# LINE 779 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 779 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asRelDir "tmp") == "tmp"
  )
 DocTest.printPrefix "System.Path.Internal:780: "
{-# LINE 780 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 780 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asRelDir "/tmp") == "tmp"
  )
 DocTest.printPrefix "System.Path.Internal:786: "
{-# LINE 786 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 786 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asAbsFile "/file.txt") == "/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:787: "
{-# LINE 787 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 787 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asAbsFile "/tmp") == "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:793: "
{-# LINE 793 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 793 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asAbsDir "/file.txt") == "/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:794: "
{-# LINE 794 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 794 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.asAbsDir "/tmp") == "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:833: "
{-# LINE 833 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 833 "src/System/Path/Internal.hs" #-}
      Path.mkPathAbsOrRel "/tmp" == Left (Posix.absDir "/tmp")
  )
 DocTest.printPrefix "System.Path.Internal:834: "
{-# LINE 834 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 834 "src/System/Path/Internal.hs" #-}
      Path.mkPathAbsOrRel  "tmp" == Right (Posix.relDir "tmp")
  )
 DocTest.printPrefix "System.Path.Internal:835: "
{-# LINE 835 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 835 "src/System/Path/Internal.hs" #-}
      Path.mkPathAbsOrRel "\\tmp" == Left (Windows.absDir "\\tmp")
  )
 DocTest.printPrefix "System.Path.Internal:836: "
{-# LINE 836 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 836 "src/System/Path/Internal.hs" #-}
      Path.mkPathAbsOrRel "d:\\tmp" == Left (Windows.absDir "d:\\tmp")
  )
 DocTest.printPrefix "System.Path.Internal:837: "
{-# LINE 837 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 837 "src/System/Path/Internal.hs" #-}
      Path.mkPathAbsOrRel "d:tmp" == Left (Windows.absDir "d:tmp")
  )
 DocTest.printPrefix "System.Path.Internal:838: "
{-# LINE 838 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 838 "src/System/Path/Internal.hs" #-}
      Path.mkPathAbsOrRel "tmp" == Right (Windows.relDir "tmp")
  )
 DocTest.printPrefix "System.Path.Internal:867: "
{-# LINE 867 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 867 "src/System/Path/Internal.hs" #-}
      Path.mkAbsPath (absDir "/tmp") "foo.txt" == Posix.absFile "/tmp/foo.txt"
  )
 DocTest.printPrefix "System.Path.Internal:868: "
{-# LINE 868 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 868 "src/System/Path/Internal.hs" #-}
      Path.mkAbsPath (absDir "/tmp") "/etc/foo.txt" == Posix.absFile "/etc/foo.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1010: "
{-# LINE 1010 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1010 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.absDir "/tmp" </> Posix.relFile "file.txt") == "/tmp/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1011: "
{-# LINE 1011 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1011 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.absDir "/tmp" </> Posix.relDir "dir" </> Posix.relFile "file.txt") == "/tmp/dir/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1012: "
{-# LINE 1012 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1012 "src/System/Path/Internal.hs" #-}
      Posix.toString (Posix.relDir "dir" </> Posix.relFile "file.txt") == "dir/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1013: "
{-# LINE 1013 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1013 "src/System/Path/Internal.hs" #-}
      Windows.toString (Windows.absDir "\\tmp" </> Windows.relFile "file.txt") == "\\tmp\\file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1014: "
{-# LINE 1014 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1014 "src/System/Path/Internal.hs" #-}
      Windows.toString (Windows.absDir "c:\\tmp" </> Windows.relFile "file.txt") == "c:\\tmp\\file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1015: "
{-# LINE 1015 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1015 "src/System/Path/Internal.hs" #-}
      Windows.toString (Windows.absDir "c:tmp" </> Windows.relFile "file.txt") == "c:tmp\\file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1016: "
{-# LINE 1016 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1016 "src/System/Path/Internal.hs" #-}
      Windows.toString (Windows.absDir "c:\\" </> Windows.relDir "tmp" </> Windows.relFile "file.txt") == "c:\\tmp\\file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1017: "
{-# LINE 1017 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1017 "src/System/Path/Internal.hs" #-}
      Windows.toString (Windows.absDir "c:" </> Windows.relDir "tmp" </> Windows.relFile "file.txt") == "c:tmp\\file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1018: "
{-# LINE 1018 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1018 "src/System/Path/Internal.hs" #-}
      Windows.toString (Windows.relDir "dir" </> Windows.relFile "file.txt") == "dir\\file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1045: "
{-# LINE 1045 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1045 "src/System/Path/Internal.hs" #-}
      Path.addExtension (relFile "file.txt") "bib" == Posix.relFile "file.txt.bib"
  )
 DocTest.printPrefix "System.Path.Internal:1046: "
{-# LINE 1046 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1046 "src/System/Path/Internal.hs" #-}
      Path.addExtension (relFile "file.") ".bib" == Posix.relFile "file..bib"
  )
 DocTest.printPrefix "System.Path.Internal:1047: "
{-# LINE 1047 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1047 "src/System/Path/Internal.hs" #-}
      Path.addExtension (relFile "file") ".bib" == Posix.relFile "file.bib"
  )
 DocTest.printPrefix "System.Path.Internal:1048: "
{-# LINE 1048 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1048 "src/System/Path/Internal.hs" #-}
      Path.addExtension Path.emptyFile "bib" == Posix.relFile ".bib"
  )
 DocTest.printPrefix "System.Path.Internal:1049: "
{-# LINE 1049 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1049 "src/System/Path/Internal.hs" #-}
      Path.addExtension Path.emptyFile ".bib" == Posix.relFile ".bib"
  )
 DocTest.printPrefix "System.Path.Internal:1050: "
{-# LINE 1050 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1050 "src/System/Path/Internal.hs" #-}
      Path.takeFileName (Path.addExtension Path.emptyFile "ext") == Posix.relFile ".ext"
  )
 DocTest.printPrefix "System.Path.Internal:1057: "
{-# LINE 1057 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1057 "src/System/Path/Internal.hs" #-}
      \p -> Path.combine Path.currentDir p == (p::Default.RelDir)
  )
 DocTest.printPrefix "System.Path.Internal:1064: "
{-# LINE 1064 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1064 "src/System/Path/Internal.hs" #-}
      forAllAbsRel $ \x -> Path.dropExtension x == fst (Path.splitExtension x)
  )
 DocTest.printPrefix "System.Path.Internal:1070: "
{-# LINE 1070 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1070 "src/System/Path/Internal.hs" #-}
      forAllAbsRel $ \x -> not $ Path.hasAnExtension (Path.dropExtensions x)
  )
 DocTest.printPrefix "System.Path.Internal:1081: "
{-# LINE 1081 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1081 "src/System/Path/Internal.hs" #-}
      Path.replaceExtension (relFile "file.txt") ".bob" == Posix.relFile "file.bob"
  )
 DocTest.printPrefix "System.Path.Internal:1082: "
{-# LINE 1082 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1082 "src/System/Path/Internal.hs" #-}
      Path.replaceExtension (relFile "file.txt") "bob" == Posix.relFile "file.bob"
  )
 DocTest.printPrefix "System.Path.Internal:1083: "
{-# LINE 1083 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1083 "src/System/Path/Internal.hs" #-}
      Path.replaceExtension (relFile "file") ".bob" == Posix.relFile "file.bob"
  )
 DocTest.printPrefix "System.Path.Internal:1084: "
{-# LINE 1084 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1084 "src/System/Path/Internal.hs" #-}
      Path.replaceExtension (relFile "file.txt") "" == Posix.relFile "file"
  )
 DocTest.printPrefix "System.Path.Internal:1085: "
{-# LINE 1085 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1085 "src/System/Path/Internal.hs" #-}
      Path.replaceExtension (relFile "file.fred.bob") "txt" == Posix.relFile "file.fred.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1102: "
{-# LINE 1102 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1102 "src/System/Path/Internal.hs" #-}
      forAllAbsRel $ \x -> uncurry (<.>) (Path.splitExtension x) == x
  )
 DocTest.printPrefix "System.Path.Internal:1103: "
{-# LINE 1103 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1103 "src/System/Path/Internal.hs" #-}
      forAllAbsRel $ \x -> uncurry Path.addExtension (Path.splitExtension x) == x
  )
 DocTest.printPrefix "System.Path.Internal:1104: "
{-# LINE 1104 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1104 "src/System/Path/Internal.hs" #-}
      Path.splitExtension (relFile "file.txt") == (Posix.relFile "file",".txt")
  )
 DocTest.printPrefix "System.Path.Internal:1105: "
{-# LINE 1105 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1105 "src/System/Path/Internal.hs" #-}
      Path.splitExtension (relFile ".bashrc") == (Posix.emptyFile, ".bashrc")
  )
 DocTest.printPrefix "System.Path.Internal:1106: "
{-# LINE 1106 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1106 "src/System/Path/Internal.hs" #-}
      Path.splitExtension (relFile "file") == (Posix.relFile "file","")
  )
 DocTest.printPrefix "System.Path.Internal:1107: "
{-# LINE 1107 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1107 "src/System/Path/Internal.hs" #-}
      Path.splitExtension (relFile "file/file.txt") == (Posix.relFile "file/file",".txt")
  )
 DocTest.printPrefix "System.Path.Internal:1108: "
{-# LINE 1108 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1108 "src/System/Path/Internal.hs" #-}
      Path.splitExtension (relFile "file.txt/boris") == (Posix.relFile "file.txt/boris","")
  )
 DocTest.printPrefix "System.Path.Internal:1109: "
{-# LINE 1109 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1109 "src/System/Path/Internal.hs" #-}
      Path.splitExtension (relFile "file.txt/boris.ext") == (Posix.relFile "file.txt/boris",".ext")
  )
 DocTest.printPrefix "System.Path.Internal:1110: "
{-# LINE 1110 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1110 "src/System/Path/Internal.hs" #-}
      Path.splitExtension (relFile "file/path.txt.bob.fred") == (Posix.relFile "file/path.txt.bob",".fred")
  )
 DocTest.printPrefix "System.Path.Internal:1116: "
{-# LINE 1116 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1116 "src/System/Path/Internal.hs" #-}
      Path.splitExtensions (relFile "file.tar.gz") == (Posix.relFile "file",".tar.gz")
  )
 DocTest.printPrefix "System.Path.Internal:1117: "
{-# LINE 1117 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1117 "src/System/Path/Internal.hs" #-}
      \p -> uncurry (<.>) (Path.splitExtension p) == (p::Default.AbsFile)
  )
 DocTest.printPrefix "System.Path.Internal:1121: "
{-# LINE 1121 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1121 "src/System/Path/Internal.hs" #-}
           \p -> uncurry Path.combine (Path.splitFileName p) == (p::Default.AbsFile)
  )
 DocTest.printPrefix "System.Path.Internal:1137: "
{-# LINE 1137 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1137 "src/System/Path/Internal.hs" #-}
      Path.takeBaseName (absFile "/tmp/somedir/myfile.txt") == Posix.relFile "myfile"
  )
 DocTest.printPrefix "System.Path.Internal:1138: "
{-# LINE 1138 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1138 "src/System/Path/Internal.hs" #-}
      Path.takeBaseName (relFile "./myfile.txt") == Posix.relFile "myfile"
  )
 DocTest.printPrefix "System.Path.Internal:1139: "
{-# LINE 1139 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1139 "src/System/Path/Internal.hs" #-}
      Path.takeBaseName (relFile "myfile.txt") == Posix.relFile "myfile"
  )
 DocTest.printPrefix "System.Path.Internal:1158: "
{-# LINE 1158 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1158 "src/System/Path/Internal.hs" #-}
      forAllAbsRel $ \x -> Path.takeExtension x == snd (Path.splitExtension x)
  )
 DocTest.printPrefix "System.Path.Internal:1159: "
{-# LINE 1159 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1159 "src/System/Path/Internal.hs" #-}
      forAllAbsRel $ \x -> Path.takeExtension (Path.addExtension x "ext") == ".ext"
  )
 DocTest.printPrefix "System.Path.Internal:1160: "
{-# LINE 1160 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1160 "src/System/Path/Internal.hs" #-}
      forAllAbsRel $ \x -> Path.takeExtension (Path.replaceExtension x "ext") == ".ext"
  )
 DocTest.printPrefix "System.Path.Internal:1166: "
{-# LINE 1166 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1166 "src/System/Path/Internal.hs" #-}
      Path.takeExtensions (Posix.relFile "file.tar.gz") == ".tar.gz"
  )
 DocTest.printPrefix "System.Path.Internal:1172: "
{-# LINE 1172 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1172 "src/System/Path/Internal.hs" #-}
      Path.takeFileName (absFile "/tmp/somedir/myfile.txt") == Posix.relFile "myfile.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1173: "
{-# LINE 1173 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1173 "src/System/Path/Internal.hs" #-}
      Path.takeFileName (relFile "./myfile.txt") == Posix.relFile "myfile.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1174: "
{-# LINE 1174 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1174 "src/System/Path/Internal.hs" #-}
      Path.takeFileName (relFile "myfile.txt") == Posix.relFile "myfile.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1175: "
{-# LINE 1175 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1175 "src/System/Path/Internal.hs" #-}
      \p -> Path.toString (Path.takeFileName p) `isSuffixOf` Path.toString (p::Default.AbsFile)
  )
 DocTest.printPrefix "System.Path.Internal:1204: "
{-# LINE 1204 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1204 "src/System/Path/Internal.hs" #-}
            Posix.equalFilePath "abc/def" "abc/def"
  )
 DocTest.printPrefix "System.Path.Internal:1205: "
{-# LINE 1205 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1205 "src/System/Path/Internal.hs" #-}
            Posix.equalFilePath "abc/def" "abc//def"
  )
 DocTest.printPrefix "System.Path.Internal:1206: "
{-# LINE 1206 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1206 "src/System/Path/Internal.hs" #-}
            Posix.equalFilePath "/tmp/" "/tmp"
  )
 DocTest.printPrefix "System.Path.Internal:1207: "
{-# LINE 1207 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1207 "src/System/Path/Internal.hs" #-}
            Posix.equalFilePath "/tmp" "//tmp"
  )
 DocTest.printPrefix "System.Path.Internal:1208: "
{-# LINE 1208 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1208 "src/System/Path/Internal.hs" #-}
            Posix.equalFilePath "/tmp" "///tmp"
  )
 DocTest.printPrefix "System.Path.Internal:1209: "
{-# LINE 1209 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1209 "src/System/Path/Internal.hs" #-}
      not $ Posix.equalFilePath "abc" "def"
  )
 DocTest.printPrefix "System.Path.Internal:1210: "
{-# LINE 1210 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1210 "src/System/Path/Internal.hs" #-}
      not $ Posix.equalFilePath "/tmp" "tmp"
  )
 DocTest.printPrefix "System.Path.Internal:1211: "
{-# LINE 1211 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1211 "src/System/Path/Internal.hs" #-}
            Windows.equalFilePath "abc\\def" "abc\\def"
  )
 DocTest.printPrefix "System.Path.Internal:1212: "
{-# LINE 1212 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1212 "src/System/Path/Internal.hs" #-}
            Windows.equalFilePath "abc\\def" "abc\\\\def"
  )
 DocTest.printPrefix "System.Path.Internal:1213: "
{-# LINE 1213 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1213 "src/System/Path/Internal.hs" #-}
            Windows.equalFilePath "file" "File"
  )
 DocTest.printPrefix "System.Path.Internal:1214: "
{-# LINE 1214 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1214 "src/System/Path/Internal.hs" #-}
            Windows.equalFilePath "\\file" "\\\\file"
  )
 DocTest.printPrefix "System.Path.Internal:1215: "
{-# LINE 1215 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1215 "src/System/Path/Internal.hs" #-}
            Windows.equalFilePath "\\file" "\\\\\\file"
  )
 DocTest.printPrefix "System.Path.Internal:1216: "
{-# LINE 1216 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1216 "src/System/Path/Internal.hs" #-}
      not $ Windows.equalFilePath "abc" "def"
  )
 DocTest.printPrefix "System.Path.Internal:1217: "
{-# LINE 1217 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1217 "src/System/Path/Internal.hs" #-}
      not $ Windows.equalFilePath "file" "dir"
  )
 DocTest.printPrefix "System.Path.Internal:1230: "
{-# LINE 1230 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1230 "src/System/Path/Internal.hs" #-}
      Path.joinPath ["tmp","someDir","dir"] == Posix.relDir "tmp/someDir/dir"
  )
 DocTest.printPrefix "System.Path.Internal:1231: "
{-# LINE 1231 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1231 "src/System/Path/Internal.hs" #-}
      Path.joinPath ["tmp","someDir","file.txt"] == Posix.relFile "tmp/someDir/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1237: "
{-# LINE 1237 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1237 "src/System/Path/Internal.hs" #-}
      Path.normalise (absFile "/tmp/fred/./jim/./file") == Posix.absFile "/tmp/fred/jim/file"
  )
 DocTest.printPrefix "System.Path.Internal:1243: "
{-# LINE 1243 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1243 "src/System/Path/Internal.hs" #-}
      Path.splitPath (Posix.absDir "/tmp/someDir/mydir.dir") == (True, map relDir ["tmp","someDir","mydir.dir"], Nothing)
  )
 DocTest.printPrefix "System.Path.Internal:1244: "
{-# LINE 1244 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1244 "src/System/Path/Internal.hs" #-}
      Path.splitPath (Posix.absFile "/tmp/someDir/myfile.txt") == (True, map relDir ["tmp","someDir"], Just $ relFile "myfile.txt")
  )
 DocTest.printPrefix "System.Path.Internal:1260: "
{-# LINE 1260 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1260 "src/System/Path/Internal.hs" #-}
      Path.makeRelative (absDir "/tmp/somedir") (absFile "/tmp/somedir/anotherdir/file.txt") == Posix.relFile "anotherdir/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1261: "
{-# LINE 1261 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1261 "src/System/Path/Internal.hs" #-}
      Path.makeRelative (absDir "/tmp/somedir") (absDir "/tmp/somedir/anotherdir/dir") == Posix.relDir "anotherdir/dir"
  )
 DocTest.printPrefix "System.Path.Internal:1262: "
{-# LINE 1262 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1262 "src/System/Path/Internal.hs" #-}
      Path.makeRelative (absDir "c:\\tmp\\somedir") (absFile "C:\\Tmp\\SomeDir\\AnotherDir\\File.txt") == Windows.relFile "AnotherDir\\File.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1263: "
{-# LINE 1263 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1263 "src/System/Path/Internal.hs" #-}
      Path.makeRelative (absDir "c:\\tmp\\somedir") (absDir "c:\\tmp\\somedir\\anotherdir\\dir") == Windows.relDir "anotherdir\\dir"
  )
 DocTest.printPrefix "System.Path.Internal:1264: "
{-# LINE 1264 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1264 "src/System/Path/Internal.hs" #-}
      Path.makeRelative (absDir "c:tmp\\somedir") (absDir "c:tmp\\somedir\\anotherdir\\dir") == Windows.relDir "anotherdir\\dir"
  )
 DocTest.printPrefix "System.Path.Internal:1291: "
{-# LINE 1291 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1291 "src/System/Path/Internal.hs" #-}
      Path.makeAbsolute (absDir "/tmp") (relFile "file.txt")      == Posix.absFile "/tmp/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1292: "
{-# LINE 1292 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1292 "src/System/Path/Internal.hs" #-}
      Path.makeAbsolute (absDir "/tmp") (relFile "adir/file.txt") == Posix.absFile "/tmp/adir/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1293: "
{-# LINE 1293 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1293 "src/System/Path/Internal.hs" #-}
      Path.makeAbsolute (absDir "/tmp") (relDir  "adir/dir")      == Posix.absDir "/tmp/adir/dir"
  )
 DocTest.printPrefix "System.Path.Internal:1294: "
{-# LINE 1294 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1294 "src/System/Path/Internal.hs" #-}
      \base p -> Default.toString p `isSuffixOf` Path.toString (Path.makeAbsolute base (Path.idFile p))
  )
 DocTest.printPrefix "System.Path.Internal:1295: "
{-# LINE 1295 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1295 "src/System/Path/Internal.hs" #-}
      \base p -> Default.toString base `isPrefixOf` Path.toString (Path.makeAbsolute base (Path.idFile p))
  )
 DocTest.printPrefix "System.Path.Internal:1317: "
{-# LINE 1317 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1317 "src/System/Path/Internal.hs" #-}
      Path.genericMakeAbsolute (absDir "/tmp") (relFile "file.txt")       == Posix.absFile "/tmp/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1318: "
{-# LINE 1318 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1318 "src/System/Path/Internal.hs" #-}
      Path.genericMakeAbsolute (absDir "/tmp") (relFile "adir/file.txt")  == Posix.absFile "/tmp/adir/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1319: "
{-# LINE 1319 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1319 "src/System/Path/Internal.hs" #-}
      Path.genericMakeAbsolute (absDir "/tmp") (absFile "/adir/file.txt") == Posix.absFile "/adir/file.txt"
  )
 DocTest.printPrefix "System.Path.Internal:1404: "
{-# LINE 1404 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1404 "src/System/Path/Internal.hs" #-}
      Path.isAbsolute (Posix.absFile "/fred")
  )
 DocTest.printPrefix "System.Path.Internal:1405: "
{-# LINE 1405 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1405 "src/System/Path/Internal.hs" #-}
      Path.isAbsolute (Windows.absFile "\\fred")
  )
 DocTest.printPrefix "System.Path.Internal:1406: "
{-# LINE 1406 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1406 "src/System/Path/Internal.hs" #-}
      Path.isAbsolute (Windows.absFile "c:\\fred")
  )
 DocTest.printPrefix "System.Path.Internal:1407: "
{-# LINE 1407 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1407 "src/System/Path/Internal.hs" #-}
      Path.isAbsolute (Windows.absFile "c:fred")
  )
 DocTest.printPrefix "System.Path.Internal:1414: "
{-# LINE 1414 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1414 "src/System/Path/Internal.hs" #-}
      Path.isRelative (Posix.relFile "fred")
  )
 DocTest.printPrefix "System.Path.Internal:1415: "
{-# LINE 1415 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1415 "src/System/Path/Internal.hs" #-}
      Path.isRelative (Windows.relFile "fred")
  )
 DocTest.printPrefix "System.Path.Internal:1440: "
{-# LINE 1440 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1440 "src/System/Path/Internal.hs" #-}
      forAllAbsRel $ \x -> null (Path.takeExtension x) == not (Path.hasAnExtension x)
  )
 DocTest.printPrefix "System.Path.Internal:1446: "
{-# LINE 1446 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1446 "src/System/Path/Internal.hs" #-}
      Path.hasExtension ".hs" (Posix.relFile "MyCode.hs")
  )
 DocTest.printPrefix "System.Path.Internal:1447: "
{-# LINE 1447 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1447 "src/System/Path/Internal.hs" #-}
      Path.hasExtension ".hs" (Posix.relFile "MyCode.bak.hs")
  )
 DocTest.printPrefix "System.Path.Internal:1448: "
{-# LINE 1448 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1448 "src/System/Path/Internal.hs" #-}
      not $ Path.hasExtension ".hs" (Posix.relFile "MyCode.hs.bak")
  )
 DocTest.printPrefix "System.Path.Internal:1458: "
{-# LINE 1458 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1458 "src/System/Path/Internal.hs" #-}
      Posix.extSeparator == '.'
  )
 DocTest.printPrefix "System.Path.Internal:1469: "
{-# LINE 1469 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1469 "src/System/Path/Internal.hs" #-}
      \a -> Posix.isExtSeparator a == (a == Posix.extSeparator)
  )
 DocTest.printPrefix "System.Path.Internal:1475: "
{-# LINE 1475 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1475 "src/System/Path/Internal.hs" #-}
      \a -> Posix.isSearchPathSeparator a == (a == Posix.searchPathSeparator)
  )
 DocTest.printPrefix "System.Path.Internal:1491: "
{-# LINE 1491 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1491 "src/System/Path/Internal.hs" #-}
      Path.genericAddExtension (absDir "/") "x" == Posix.absDir "/.x"
  )
 DocTest.printPrefix "System.Path.Internal:1492: "
{-# LINE 1492 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1492 "src/System/Path/Internal.hs" #-}
      Path.genericAddExtension (absDir "/a") "x" == Posix.absDir "/a.x"
  )
 DocTest.printPrefix "System.Path.Internal:1493: "
{-# LINE 1493 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1493 "src/System/Path/Internal.hs" #-}
      Path.genericAddExtension Path.emptyFile "x" == Posix.relFile ".x"
  )
 DocTest.printPrefix "System.Path.Internal:1494: "
{-# LINE 1494 "src/System/Path/Internal.hs" #-}
 DocTest.property(
{-# LINE 1494 "src/System/Path/Internal.hs" #-}
      Path.genericAddExtension Path.emptyFile "" == Posix.emptyFile
  )
