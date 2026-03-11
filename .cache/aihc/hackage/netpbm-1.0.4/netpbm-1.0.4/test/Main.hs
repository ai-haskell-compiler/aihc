{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable as S
import           Foreign.Ptr (castPtr)
import           Foreign.Marshal.Utils (copyBytes)
import           Test.Hspec
import           Test.HUnit

import Graphics.Netpbm


checkSinglePPM :: PPMType -> (Int, Int) -> PpmParseResult -> Assertion
checkSinglePPM typ size parseResult = case parseResult of
  Right ([PPM { ppmHeader = PPMHeader { ppmType, ppmWidth, ppmHeight } }], rest) -> (ppmType, (ppmWidth, ppmHeight), rest) `shouldBe` (typ, size, Nothing)
  Right (ppms, _)                                      -> assertFailure $ "expected only one image, but got " ++ show (length ppms)
  Left e                                               -> assertFailure $ "image parse failed: " ++ e


checkSinglePPMdata :: PPMType -> (Int, Int) -> [Int] -> PpmParseResult -> Assertion
checkSinglePPMdata typ size expected parseResult = case parseResult of
  Right ([PPM { ppmHeader = PPMHeader { ppmType, ppmWidth, ppmHeight }
              , ppmData }], rest) -> do
                                        (ppmType, (ppmWidth, ppmHeight), rest) `shouldBe` (typ, size, Nothing)
                                        pixelDataToIntList ppmData `shouldBe` expected
  Right (ppms, _)                 -> assertFailure $ "expected only one image, but got " ++ show (length ppms)
  Left e                          -> assertFailure $ "image parse failed: " ++ e


shouldNotParse :: PpmParseResult -> Assertion
shouldNotParse res = case res of
  Left _ -> return ()
  Right r -> assertFailure $ "should not parse, but parses as: " ++ show r


parse :: FilePath -> IO PpmParseResult
parse f = parsePPM <$> BS.readFile f


parseTestFile :: String -> String -> (PpmParseResult -> Assertion) -> Spec
parseTestFile name desc check = it (desc ++ " (" ++ name ++ ")") $
  parse ("test/ppms/" ++ name) >>= check


repcat :: Int -> [a] -> [a]
repcat n = concat . replicate n


-- @dir@ must have trailing slash.
checkDirectory :: FilePath -> String -> PPMType -> [(String, (Int, Int))] -> Spec
checkDirectory dir desc typ filesWithSizes = forM_ filesWithSizes $ \(f, size) ->
  parseTestFile (dir ++ f) desc $ checkSinglePPM typ size


main :: IO ()
main = hspec $ do
  describe "P6 PPM (color binary)" $ do

    parseTestFile "gimp.ppm" "a file produced by GIMP" $
      checkSinglePPM P6 (640,400)

    parseTestFile "gitlogo.ppm" "a file produced by convert" $
      checkSinglePPM P6 (220,92)

    parseTestFile "image.ppm" "some random file from the internet" $
      checkSinglePPM P6 (1200,1200)

    parseTestFile "testimg.ppm" "the color file from the netpbm test suite" $
      checkSinglePPM P6 (227,149)

    describe "more test files from the internet" $ do
      checkDirectory "internet/set1/" "from the internet" P6
        [ ("boxes_1.ppm", (63,63))
        , ("boxes_2.ppm", (63,63))
        , ("house_1.ppm", (111,132))
        , ("house_2.ppm", (111,132))
        , ("moreboxes_1.ppm", (63,63))
        , ("moreboxes_2.ppm", (63,63))
        , ("sign_1.ppm", (99,99))
        , ("sign_2.ppm", (99,99))
        , ("stop_1.ppm", (99,99))
        , ("stop_2.ppm", (99,99))
        , ("synth_1.ppm", (100,100))
        , ("synth_2.ppm", (100,100))
        , ("tree_1.ppm", (133,133))
        , ("tree_2.ppm", (133,133))
        , ("west_1.ppm", (366,216))
        , ("west_2.ppm", (366,216))
        ]
      checkDirectory "internet/set3/" "from the internet, PNM" P6
        [ ("birch.pnm", (128,128))
        , ("cotton.pnm", (256,170))
        , ("oak.pnm", (128,128))
        , ("quilt.pnm", (256,237))
        ]

      parseTestFile "internet/set2/mandrill.ppm" "the color file from the 'Math 625' course" $
        checkSinglePPM P6 (512,512)

      parseTestFile "internet/set2/half.ppm" "the color file from the 'Math 625' course, half width" $
        checkSinglePPM P6 (256,512)

      parseTestFile "SIPI.ppm" "SIPI test file" $
        -- convert SIPI.tiff SIPI.ppm
        checkSinglePPM P6 (256,256)


    parseTestFile "gitlogo-double.ppm" "a multi-image file" $ do
      \res -> case res of
        Right ([ PPM { ppmHeader = h1 }
               , PPM { ppmHeader = h2 }], rest) -> do h1 `shouldBe` PPMHeader P6 220 92
                                                      h2 `shouldBe` PPMHeader P6 220 92
                                                      rest `shouldBe` Nothing
        Right r                                    -> assertFailure $ "parsed unexpected: " ++ show r
        Left e                                     -> assertFailure $ "did not parse: " ++ e


    describe "comments" $ do

      parseTestFile "gitlogo-comments.ppm" "comments as a sane user would write them" $
        checkSinglePPM P6 (220,92)

      parseTestFile "gitlogo-comment-after-magic-number.ppm" "a comment directly after the P6" $
        checkSinglePPM P6 (220,92)

      parseTestFile "gitlogo-only-spaces-in-header.ppm" "only spaces as header separators" $
        checkSinglePPM P6 (220,92)

      parseTestFile "gitlogo-comment-is-data.ppm" "the user thinks they wrote a comment, but it's actually parsed as data" $
        \res -> case res of
          Right ([PPM { ppmHeader }], Just rest) -> do ppmHeader `shouldBe` PPMHeader P6 220 92
                                                       rest `shouldBe` "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\n"
          Right r                                -> assertFailure $ "parsed unexpected: " ++ show r
          Left e                                 -> assertFailure $ "did not parse: " ++ e


    describe "weird files that are still OK with the spec" $ do

      parseTestFile "weird/gitlogo-width-0.ppm" "width '00' set in an image" $
        \res -> case res of
          Right ([PPM { ppmHeader }], Just rest) -> do ppmHeader `shouldBe` PPMHeader P6 220 0
                                                       assertBool "missing rest" (BS.length rest > 200)
          Right r                                -> assertFailure $ "parsed unexpected: " ++ show r
          Left e                                 -> assertFailure $ "did not parse: " ++ e

      parseTestFile "weird/gitlogo-comments-everywhere.ppm" "comments inside numbers" $
        checkSinglePPM P6 (220,92)


    describe "partially valid files of which we parse as much as we can" $ do

      parseTestFile "graceful/face.ppm" "a PPM with a trailing newline" $
        checkSinglePPM P6 (512,512)

      parseTestFile "graceful/gitlogo-one-and-a-half.ppm" "a multi-image file where the second image is chopped off" $
        \res -> case res of
          Right ([PPM { ppmHeader }], Just rest) -> do ppmHeader `shouldBe` PPMHeader P6 220 92
                                                       assertBool "missing rest" (BS.length rest > 200)
          Right r                                -> assertFailure $ "parsed unexpected: " ++ show r
          Left e                                 -> assertFailure $ "did not parse: " ++ e

      parseTestFile "graceful/gitlogo-double-with-whitespace-in-between.ppm" "a multi-image file with whitespace between the images" $
        \res -> case res of
          Right ([ PPM { ppmHeader = h1 }
                 , PPM { ppmHeader = h2 }], rest) -> do h1 `shouldBe` PPMHeader P6 220 92
                                                        h2 `shouldBe` PPMHeader P6 220 92
                                                        rest `shouldBe` Nothing
          Right r                                 -> assertFailure $ "parsed unexpected: " ++ show r
          Left e                                  -> assertFailure $ "did not parse: " ++ e


    describe "16-bit images" $ do

      -- See http://wiki.simg.de/doku.php?id=common:formats#pnm_family
      parseTestFile "gitlogo-16bit-created-by-simg_convert_-16be_gitlogo.ppm_output.ppm" "16-bit image created by simg" $
        \res -> case res of
          Right ([PPM { ppmHeader, ppmData }], rest) -> do ppmHeader `shouldBe` PPMHeader P6 220 92
                                                           rest `shouldBe` Nothing
                                                           case ppmData of
                                                             PpmPixelDataRGB16 _ -> return ()
                                                             _                   -> assertFailure $ "did not get 16-bit data"
          Right r                                 -> assertFailure $ "parsed unexpected: " ++ show r
          Left e                                  -> assertFailure $ "did not parse: " ++ e

      parseTestFile "SIPI-16.ppm" "SIPI test file" $
        -- convert SIPI.tiff -depth 16 SIPI-16.ppm
        checkSinglePPM P6 (256,256)

      -- TODO try to get a 16-bit image out of the new gimp

    describe "negative examples" $ do

      parseTestFile "bad/gitlogo-garbage-in-numbers.ppm" "ascii characters in a number" shouldNotParse

      parseTestFile "bad/gitlogo-width--1.ppm" "width '-1' set in an image" shouldNotParse

      parseTestFile "bad/gitlogo-not-enough-data.ppm" "not containing (width * height) bytes" shouldNotParse

      parseTestFile "bad/gitlogo-comment-in-magic-number.ppm" "comment inside magic number" shouldNotParse

      parseTestFile "bad/gitlogo-comment-user-error.ppm" "a comment accidentally being put to close to a number, eating the following whitespace" $ shouldNotParse

      parseTestFile "bad/gitlogo-comment-user-error-no-space-after-magic.ppm" "a comment accidentally being put to close to the magic number, eating the following whitespace" $ shouldNotParse

      parseTestFile "bad/gitlogo-comment-without-following-extra-newline-before-data-block.ppm" "no non-comment whitespace before data block" shouldNotParse

      parseTestFile "bad/gitlogo-value-bigger-than-maxval.ppm" "subpixel value is bigger than maxval" shouldNotParse

      parseTestFile "internet/set3/cathedral.pnm" "subpixel value is bigger than maxval" shouldNotParse
      parseTestFile "internet/set3/checkers.pnm"  "subpixel value is bigger than maxval" shouldNotParse
      parseTestFile "internet/set3/fish_tile.pnm" "subpixel value is bigger than maxval" shouldNotParse
      parseTestFile "internet/set3/garnet.pnm"    "subpixel value is bigger than maxval" shouldNotParse


  describe "P5 PGM (greyscale binary)" $ do

    parseTestFile "internet/set2/mandrill.pgm" "the color file from the 'Math 625' course" $
      checkSinglePPM P5 (512,512)

    parseTestFile "internet/set2/half.pgm" "the color file from the 'Math 625' course, half width" $
      checkSinglePPM P5 (256,512)

    parseTestFile "SIPI-convert.pgm" "a file produced by convert" $
      -- convert SIPI.tiff SIPI-convert.pgm
      checkSinglePPM P5 (256,256)


    describe "comments" $ do

      parseTestFile "internet/set2/comments.pgm" "the color file from the 'Math 625' course, with comments" $
        checkSinglePPM P5 (512,512)


    describe "16-bit" $ do

      parseTestFile "SIPI-convert-16.pgm" "a file produced by convert, 16-bit" $
        -- convert SIPI.tiff -depth 16 SIPI-convert-16.pgm
        checkSinglePPM P5 (256,256)


  describe "P4 PBM (bitmap binary)" $ do

    parseTestFile "testgrid.pbm" "the bitmap file from the netpbm test suite" $
      checkSinglePPMdata P4 (14,16) (repcat 8 (repcat 7 [0,1] ++ replicate 14 0))

    parseTestFile "SIPI-convert.pbm" "a file produced by convert" $
      -- convert SIPI.tiff SIPI-convert.pbm
      checkSinglePPM P4 (256,256)


  describe "P3 PPM (color ASCII)" $ do

    checkDirectory "internet/set3/" "more test files from the internet" P3
      [ ("feep.ppm", (4,4))
      , ("snail.ppm", (256,256))
      ]

    parseTestFile "SIPI-convert-plain.ppm" "a file produced by convert" $
      -- convert SIPI.tiff -compress none SIPI-convert-plain.ppm
      checkSinglePPM P3 (256,256)

    parseTestFile "SIPI-convert-plain-16.ppm" "a file produced by convert, 16-bit" $
      -- convert SIPI.tiff -compress none -depth 16 SIPI-convert-plain-16.ppm
      checkSinglePPM P3 (256,256)


  describe "P2 PGM (greyscale ASCII)" $ do

    checkDirectory "internet/set3/" "more test files from the internet" P2
      [ ("balloons.pgm", (640,480))
      , ("columns.pgm", (640,480))
      , ("feep.pgm", (24,7))
      , ("tracks.pgm", (300,200))
      ]

    parseTestFile "pgm-plain-made-up-from-pbm-spec.pgm" "the plain PBM file from the spec example, converted to PGM" $
      -- Invert 0/1 because in PBM 1 is black, not so in PGM
      checkSinglePPMdata P2 (24,7) (map (1 -) pbmFromSpecResult)

    parseTestFile "SIPI-convert-plain.pgm" "a file produced by convert" $
      -- convert SIPI.tiff -compress none SIPI-convert-plain.pgm
      checkSinglePPM P2 (256,256)

    describe "16-bit" $ do

      parseTestFile "SIPI-convert-plain-16.pgm" "a file produced by convert, 16-bit" $
        -- convert SIPI.tiff -compress none -depth 16 SIPI-convert-plain-16.pgm
        checkSinglePPM P2 (256,256)


  describe "P1 PBM (bitmap ASCII)" $ do

    describe "more test files from the internet" $ do
      checkDirectory "internet/set3/" "from the internet" P1
        [ ("circle_ascii.pbm", (200,200))
        , ("feep.pbm", (24,7))
        ]

    parseTestFile "pbm-plain-from-spec.pbm" "the plain PBM file from the spec example" $
      checkSinglePPMdata P1 (24,7) pbmFromSpecResult


    describe "ASCII files should only contain one image" $ do

      parseTestFile "pbm-plain-from-spec-multiple-but-treated-as-junk.pbm" "ASCII PBM from spec, multiple times, rest should be treated as junk" $
        checkSinglePPMdata P1 (24,7) pbmFromSpecResult

      parseTestFile "bad/pbm-plain-from-spec-multiple-no-space-before-junk.pbm" "ASCII PBM from spec, multiple times, rest should be treated as junk" $
        shouldNotParse


    parseTestFile "SIPI-convert-plain.pbm" "a file produced by convert" $
      -- convert SIPI.tiff -compress none SIPI-convert-plain.pbm
      checkSinglePPM P1 (256,256)


  describe "In-memory format" $ do

    it "represents 8-bit images in rgbrgbrgb... format" $ do

      let d = PpmPixelDataRGB8 $ S.fromList [ PpmPixelRGB8 1 2 3
                                            , PpmPixelRGB8 4 5 6
                                            , PpmPixelRGB8 7 8 9 ]
      let PpmPixelDataRGB8 vec = d

      bs <- BSI.create 9 $ \bsPtrWord8 ->
        S.unsafeWith vec $ \ppmPixelRgb8Ptr ->
          copyBytes bsPtrWord8 (castPtr ppmPixelRgb8Ptr) 9

      bs `shouldBe` "\1\2\3\4\5\6\7\8\9"


-- Some result data

-- Note that in a PBM file, "1" means black, but in the result 0 means black.
pbmFromSpecResult :: [Int]
pbmFromSpecResult = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
                    ,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1
                    ,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,0,1
                    ,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,1
                    ,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1
                    ,1,0,1,1,1,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,1,1
                    ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
