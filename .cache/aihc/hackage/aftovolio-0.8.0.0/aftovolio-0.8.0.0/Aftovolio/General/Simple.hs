{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Aftovolio.General.Simple where

import Aftovolio.ConstraintsEncoded
import Aftovolio.General.Base
import Aftovolio.General.Datatype3
import Aftovolio.General.Distance
import Aftovolio.General.PrepareText
import Aftovolio.General.Syllables
import Aftovolio.Halfsplit
import Aftovolio.StrictVG
import Aftovolio.PermutationsArr
import Aftovolio.PermutationsArrMini1
import Aftovolio.PermutationsArrMini2
import Aftovolio.PermutationsArrMini
import Aftovolio.PermutationsRepresent
import Aftovolio.Tests
import Aftovolio.UniquenessPeriodsG
import CLI.Arguments
import CLI.Arguments.Get
import CLI.Arguments.Parsing
import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq
import Data.Char (isDigit, isSpace)
import Data.ChooseLine2
import Data.List hiding (foldr, null)
import qualified Data.List as L (null,lines)
import Data.Lists.FLines (newLineEnding)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, mapMaybe)
import Data.MinMax1 (minMax11By)
import Data.Ord (Down (..), comparing)
import Data.ReversedScientific
import Data.Tuple (fst)
import GHC.Base
import GHC.Enum (fromEnum)
import GHC.Generics
import GHC.Int (Int8)
import GHC.Num (Integer, Num, (*), (+), (-))
import GHC.Real (Integral, fromIntegral, gcd, quot, quotRem, rem, round, (/), (^))
import GHC.Word
import GHC.Word (Word8)
import Numeric (showFFloat)
import Rhythmicity.MarkerSeqs hiding (id)
import System.Directory (
    Permissions (..),
    doesFileExist,
    getCurrentDirectory,
    getPermissions,
    readable,
    writable,
 )
import System.Environment (getArgs)
import System.IO (
    FilePath,
    appendFile,
    getLine,
    hSetNewlineMode,
    putStr,
    putStrLn,
    readFile,
    stdout,
    universalNewlineMode,
    writeFile,
 )
import Text.Read (readMaybe)
import Text.Show (Show (..))

generalF ::
    -- | A power of 10. The resulting distance using next ['Word8'] argument is quoted by 10 in this power. The default one is 0. The proper values are in the range [0..4].
    Int ->
    -- | A 'length' of the next argument here.
    Int ->
    -- | A value that the different options are compared with. If no command line argument \"+di\" was added, then this is a `C1` applied to the list of positive values normed by 255 (the greatest of which is 255) that the line options are compared with. If null, then the program works without comparison. The length of it must be a least common multiplier of the (number of syllables plus number of \'_digits\' groups) to work correctly. Is not used when the next 'FilePath' and 'String' arguments are not null. If \"+di\" command line argument was  provided, then this corresponds to the case of differentiation.
    Compards ->
    -- | If 'True' then adds \"<br>\" to line endings for double column output
    Bool ->
    -- | Whether to filter out all groups of \'={digits}\' from the lines.
    Bool ->
    -- | A path to the file to save double columns output to. If empty then just prints to 'stdout'.
    FilePath ->
    -- | If not null than instead of rhythmicity evaluation using hashes and and feets, there is computed a diversity property for the specified 'String' here using the 'selectSounds' function. For more information, see: 'https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#types'
    String ->
    -- | A function that specifies what 'Char's in the list the first argument makes to be the function sensitive to. Analogue of the @g@ function in the definition: https://hackage.haskell.org/package/phonetic-languages-simplified-examples-array-0.21.0.0/docs/src/Phonetic.Languages.Simplified.Array.Ukrainian.FuncRep2RelatedG2.html#selectSounds. Use just small 'Char' if they are letters, do not use \'.\' and spaces.
    (String -> String) ->
    -- | If the next element is not equal to -1, then the prepending and appending lines to be displayed. Used basically for working with the multiline textual input data.
    (String, String) ->
    -- | The number of the line in the file to be read the lines from. If equal to -1 then neither reading from the file is done nor the first argument influences the processment results.
    Int ->
    -- | Data used to obtain the phonetic language representation of the text.
    GWritingSystemPRPLX ->
    -- | The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly.
    [(Char, Char)] ->
    CharPhoneticClassification ->
    SegmentRulesG ->
    -- | Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
    BasicSpaces ->
    -- | Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
    AdditionalDelimiters ->
    -- | See the conversion function 'Aftovolio.General.Datatype3.zippedDouble2Word8'. It is easier to obtain the function @f::[[[PhoneticPhenomenonRep]]]->[[Double]]@, and, afterwards, you can use 'zippedDouble2Word8' to transform the main semantic kernel of [(PhoneticPhenomenonRep, Double)] into [(PhoneticPhenomenonRep, Word8)]. For more information, see 'https://hackage.haskell.org/package/aftovolio-0.6.2.0/src/README.md' in the section 'Ability to use your own durations of representations of sounds or phonetic phenomena'.
    ([[[PhoneticPhenomenonRep]]] -> [[Word8]]) ->
    Int ->
    HashCorrections ->
    (Int8, [Int8]) ->
    -- | The hashing function step. The default value is 20. Is expected to be greater than 2, and better greater than 12.
    Int ->
    Bool ->
    Int8 ->
    (FilePath, Int) ->
    -- | In the testing mode, whether to execute computations in concurrent mode (for speed up) or in single thread. If specified needs the executable to be compiled with -rtsopts and -threaded options and run with the command line +RTS -N -RTS options.
    Bool ->
    -- | An initial string to be analyzed.
    String ->
    -- | A list of line numbers of the Aftovolio data to be displayed in the modes except tests and file appending.
    [Int] -> 
    [String] ->
    IO [String]
generalF power10 ldc compards html filtering dcfile selStr selFun (prestr, poststr) lineNmb writingSystem allophones arrCharClassification segmentRules basicSpaces additionalDelims h numTest hc (grps, mxms) hashStep emptyline splitting (fs, code) concurrently initstr lineNumbersSel universalSet
    | L.null universalSet = do
        let strOutput =
                [ "You have specified the data and constraints on it that lead to no further possible options."
                , "Please, specify another data and constraints."
                ]
        putStrLn . unlines $ strOutput
        return strOutput
    | length universalSet == 1 = do
        putStrLn . unlines $ universalSet
        return universalSet
    | otherwise = do
        let syllN = countSyll writingSystem arrCharClassification basicSpaces additionalDelims initstr
            f ldc compards grps mxms
                | L.null selStr =
                    ( if doubleFunc (L.null :: [Word8] -> Bool) (L.null :: [Int8] -> Bool) compards
                        then (sum . countHashes2G hashStep hc grps mxms)
                        else
                            (`quot` 10 ^ power10)
                                . fromIntegral
                                . sumAbsDistNormComp compards
                                . (if isWord8Based compards then C1 else C2 . fromSmallWord8toInt8Diff)
                    )
                        . read3
                            (not . L.null . filter (not . isSpace))
                            1.0
                            (mconcat . h . createSyllablesPL writingSystem allophones arrCharClassification segmentRules basicSpaces additionalDelims)
                | otherwise =
                    fromIntegral
                        . diverse2GGL (selectSounds selFun selStr) (basicSpaces `mappend` additionalDelims)
                        . concatMap string1
                        . stringToXG writingSystem
                        . filter (\c -> not (isDigit c) && c /= '_' && c /= '=')
        hSetNewlineMode stdout universalNewlineMode
        if numTest
            >= 0
            && numTest
            <= 179
            && numTest
            /= 1
            && doubleFunc (L.null :: [Word8] -> Bool) (L.null :: [Int8] -> Bool) compards
            then testsOutput concurrently syllN filtering f ldc numTest universalSet
            else
                let lgth = length universalSet
                    sRepresent =
                        zipWith (\k (x, ys) -> S k x ys) [1 ..]
                            . sortOn id
                            . map (\xss -> (f ldc compards grps mxms xss, xss))
                            $ universalSet
                    strOutput 
                        | L.null lineNumbersSel =
                        force
                            . L.lines
                            . halfsplit1G
                                (\(S _ y _) -> y)
                                filtering
                                (if html then "<br>" else "")
                                (jjj splitting) $
                            sRepresent
                        | otherwise = 
                         force 
                             . filter (not . L.null)
                             . map (\ (S k _ qqs) -> if (k `elem` lineNumbersSel && k <= lgth) then qqs else [])
                             $ sRepresent
                    lns1 = unlines strOutput
                 in do
                        if L.null lineNumbersSel then do
                             putStrLn lns1
                             if L.null dcfile
                                 then putStr ""
                                 else do
                                     doesFileExist dcfile >>= \exist ->
                                         if exist
                                             then do
                                                 getPermissions dcfile >>= \perms ->
                                                     if writable perms
                                                         then writeFile dcfile lns1
                                                         else
                                                             error $
                                                                 "Aftovolio.General.IO.generalF: File "
                                                                     `mappend` dcfile
                                                                     `mappend` " is not writable!"
                                             else do
                                                 getCurrentDirectory >>= \currdir -> do
                                                     getPermissions currdir >>= \perms ->
                                                         if writable perms
                                                             then writeFile dcfile lns1
                                                             else
                                                                 error $
                                                                     "Aftovolio.General.IO.generalF: Directory of the file "
                                                                         `mappend` dcfile
                                                                         `mappend` " is not writable!"
                             let l1 = length sRepresent
                             if code == -1
                                 then
                                     if lineNmb == -1
                                         then return strOutput
                                         else do
                                             print23 filtering prestr poststr 1 [initstr]
                                             return strOutput
                                 else do
                                     print23 filtering prestr poststr 1 [initstr]
                                     parseLineNumber l1 >>= \num -> do
                                         permiss <- getPermissions fs
                                         let writ = writable permiss
                                             readab = readable permiss
                                         if writ && readab
                                             then outputWithFile h writingSystem allophones arrCharClassification segmentRules basicSpaces additionalDelims selStr compards sRepresent code grps fs num
                                             else
                                                 error
                                                     "The specified file cannot be used for appending the text! Please, specify another file!"
                                         return strOutput
                        else mapM putStrLn strOutput >> return strOutput          
  where
    jjj kk = let (q1, r1) = quotRem kk (if kk < 0 then -10 else 10) in jjj' q1 r1 emptyline
    jjj' q1 r1 emptyline
        | r1 == (-1) || r1 == (-3) = -10 * q1 + (if emptyline then -5 else r1)
        | r1 == 1 || r1 == 3 = 10 * q1 + (if emptyline then 5 else r1)
        | r1 < 0 = -10 * q1 + (if emptyline then -4 else r1)
        | otherwise = 10 * q1 + (if emptyline then 4 else r1)

data AftovolioGen = S !Int !Integer !String deriving (Eq, Generic)

instance Show AftovolioGen where
    show (S i j xs) =
        showBignum 7 j
            `mappend` " "
            `mappend` xs
            `mappend` "  "
            `mappend` showWithSpaces 6 i -- changed for +x command line option

instance NFData AftovolioGen

countSyll ::
    -- | Data used to obtain the phonetic language representation of the text.
    GWritingSystemPRPLX ->
    CharPhoneticClassification ->
    -- | Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
    BasicSpaces ->
    -- | Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
    AdditionalDelimiters ->
    String ->
    Int
countSyll writingSystem arrCharClassification basicSpaces additionalDelims xs =
    numUnderscoresSyll
        + ( fromEnum
                . foldr (\x y -> if createsSyllable x then y + 1 else y) 0
                . concatMap (str2PRSs arrCharClassification)
                . words1
                . mapMaybe g
                . concatMap string1
                . stringToXG writingSystem $
                xs
          )
  where
    numUnderscoresSyll =
        length
            . filter
                ( \xs -> let (ys, ts) = splitAt 1 xs in ys == "_" && all isDigit ts && not (L.null ts)
                )
            . groupBy (\x y -> x == '_' && isDigit y) $
            xs
    g :: Char -> Maybe Char
    g x
        | x `elem` basicSpaces = Nothing
        | x `notElem` additionalDelims = Just x
        | otherwise = Just ' '
    words1 xs = if L.null ts then [] else w : words1 s'' -- Practically this is an optimized version for this case 'words' function from Prelude.
      where
        ts = dropWhile (== ' ') xs
        (w, s'') = break (== ' ') ts
    {-# NOINLINE words1 #-}

stat1 :: Int -> (Int8, [Int8]) -> Int
stat1 n (k, ks) = fst (n `quotRemInt` fromEnum k) * length ks

outputSel :: AftovolioGen -> Int -> String
outputSel (S x1 y1 ts) code
    | code < 0 = []
    | code == 1 || code == 11 || code == 16 =
        intercalate " " [show x1, ts] `mappend` newLineEnding
    | code == 2 || code == 12 || code == 17 =
        intercalate " " [show y1, ts] `mappend` newLineEnding
    | code == 3 || code == 13 || code == 18 =
        intercalate " " [show x1, ts, show y1] `mappend` newLineEnding
    | code == 4 || code == 14 || code == 19 =
        intercalate " " [show x1, show y1] `mappend` newLineEnding
    | otherwise = ts `mappend` newLineEnding

parseLineNumber :: Int -> IO Int
parseLineNumber l1 = do
    putStrLn
        "Please, specify the number of the option to be written to the file specified: "
    number <- getLine
    let num = readMaybe (filter isDigit number) :: Maybe Int
    if isNothing num || num > Just l1 || num == Just 0
        then parseLineNumber l1
        else return . fromJust $ num

{- | Uses 'getArgs' inside to get the needed data from the command line arguments. Use with this in
 mind.
-}
argsProcessing ::
    -- | Data used to obtain the phonetic language representation of the text.
    GWritingSystemPRPLX ->
    -- | The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly.
    [(Char, Char)] ->
    CharPhoneticClassification ->
    SegmentRulesG ->
    -- | Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
    BasicSpaces ->
    -- | Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
    AdditionalDelimiters ->
    -- | See the conversion function 'Aftovolio.General.Datatype3.zippedDouble2Word8'. It is easier to obtain the function @f::[[[PhoneticPhenomenonRep]]]->[[Double]]@, and, afterwards, you can use 'zippedDouble2Word8' to transform the main semantic kernel of [(PhoneticPhenomenonRep, Double)] into [(PhoneticPhenomenonRep, Word8)]. For more information, see 'https://hackage.haskell.org/package/aftovolio-0.6.2.0/src/README.md' in the section 'Ability to use your own durations of representations of sounds or phonetic phenomena'.
    ([[[PhoneticPhenomenonRep]]] -> [[Word8]]) ->
    [[String]] -> -- ^ Is intended to become a valid 'Concatenations' that are to be prepended to the next word.
    [[String]] -> -- ^ Is intended to become a valid 'Concatenations' that are to be appended to the previous word
    String ->
    -- | These ones are intended to be used inside 'generalF'.
    IO
        ( Int
        , Int
        , Compards
        , Bool
        , Bool
        , FilePath
        , String
        , String
        , String
        , Int
        , Bool
        , Int8
        , FilePath
        , Int
        , Bool
        , String
        , [Int]
        , [String]
        )
argsProcessing writingSystem allophones arrCharClassification segmentRules basicSpaces additionalDelims h ysss zsss xs = do
    args0 <- getArgs
    let (argsC, args) = takeCs1R ('+', '-') cSpecs args0
        (argsB, args11) = takeBsR bSpecs args
        compareByLinesFinalFile = concat . getB "-cm" $ argsB
    if not . L.null $ compareByLinesFinalFile
        then do
            compareFilesToOneCommon 14 args11 compareByLinesFinalFile
            return (0, 0, (C1 []), False, False, [], [], [], [], 0, False, 0, [], 0, False, [], [], [])
        else do
            let prepare = any (== "-p") args11
                emptyline = any (== "+l") args11
                splitting = fromMaybe 54 (readMaybe (concat . getB "+w" $ argsB) :: Maybe Int8)
                concurrently = any (== "-C") args11
                filtering = any (== "-e") args11
                dcspecs = getB "+dc" argsB
                (html, dcfile)
                    | L.null dcspecs = (False, "")
                    | otherwise = (head dcspecs == "1", last dcspecs)
                selStr = concat . getB "+ul" $ argsB
                filedata = getB "+f" argsB
                power10' = fromMaybe 0 (readMaybe (concat . getB "+q" $ argsB) :: Maybe Int)
                power10
                    | power10' < 0 && power10' > 4 = 0
                    | otherwise = power10'
                (multiline2, multiline2LineNum)
                    | oneB "+m3" argsB =
                        let r1ss = getB "+m3" argsB
                         in if length r1ss == 3
                                then
                                    let (kss, qss) = splitAt 2 r1ss
                                     in (kss, max 1 (fromMaybe 1 (readMaybe (concat qss) :: Maybe Int)))
                                else (r1ss, 1)
                    | oneB "+m2" argsB =
                        ( getB "+m" argsB
                        , max 1 (fromMaybe 1 (readMaybe (concat . getB "+m2" $ argsB) :: Maybe Int))
                        )
                    | otherwise = (getB "+m" argsB, -1)
                (fileread, lineNmb)
                    | L.null multiline2 = ("", -1)
                    | length multiline2 == 2 =
                        (head multiline2, fromMaybe 1 (readMaybe (last multiline2) :: Maybe Int))
                    | otherwise = (head multiline2, 1)
            (arg3s, prestr, poststr, linecomp3) <- do
                if lineNmb /= -1
                    then do
                        txtFromFile <- readFile fileread
                        let lns = lines txtFromFile
                            ll1 = length lns
                            ln0 = max 1 (min lineNmb (length lns))
                            lm3
                                | multiline2LineNum < 1 = -1
                                | otherwise = max 1 . min multiline2LineNum $ ll1
                            linecomp3
                                | lm3 == -1 = []
                                | otherwise = lns !! (lm3 - 1)
                            ln_1
                                | ln0 == 1 = 0
                                | otherwise = ln0 - 1
                            ln1
                                | ln0 == length lns = 0
                                | otherwise = ln0 + 1
                            lineF = lns !! (ln0 - 1)
                            line_1F
                                | ln_1 == 0 = []
                                | otherwise = lns !! (ln_1 - 1)
                            line1F
                                | ln1 == 0 = []
                                | otherwise = lns !! (ln1 - 1)
                        return $ (words lineF, line_1F, line1F, linecomp3)
                    else return (args11, [], [], [])
            let differentiate = any (== "+di") args11
                line2comparewith
                    | oneC "+l2" argsC || L.null linecomp3 = unwords . getC "+l2" $ argsC
                    | otherwise = linecomp3
                basecomp
                    | oneC "+ln" argsC =
                        (if differentiate then C2 . fromSmallWord8toInt8Diff else C1)
                            . catMaybes
                            . map (\xs -> readMaybe xs :: Maybe Word8)
                            . getC "+ln" $
                            argsC -- to read positive Word8 values as a list of them.
                    | otherwise =
                        (if differentiate then C2 . fromSmallWord8toInt8Diff else C1)
                            . read3
                                (not . L.null . filter (not . isSpace))
                                1.0
                                (mconcat . h . createSyllablesPL writingSystem allophones arrCharClassification segmentRules basicSpaces additionalDelims) $
                            line2comparewith
                (filesave, codesave)
                    | L.null filedata = ("", -1)
                    | length filedata == 2 =
                        (head filedata, fromMaybe 0 (readMaybe (last filedata) :: Maybe Int))
                    | otherwise = (head filedata, 0)
                ll =
                    let maxWordsNum = (if any (== "+x") arg3s then 9 else 7)
                     in take maxWordsNum
                            . ( if prepare
                                    then id
                                    else words . mconcat . prepareTextN maxWordsNum ysss zsss xs . unwords
                              ) $
                            arg3s
                l = length ll
                lineNumbersSel = readNums . getC "+nm" $ argsC
                argCs = catMaybes (fmap (readMaybeECG l) . getC "+a" $ argsC)
                argCBs = unwords . getC "+b" $ argsC -- If you use the parenthese with +b ... -b then consider also using the quotation marks for the whole algebraic constraint. At the moment though it is still not working properly for parentheses functionality. The issue should be fixed in the further releases.
                permutationsType = bTransform2Perms . getB "+P" $ argsB
                !perms
                    | not (L.null argCBs) = filterGeneralConv l argCBs . permChoose permutationsType $ l
                    | L.null argCs = permChoose permutationsType l
                    | otherwise = decodeLConstraints argCs . permChoose permutationsType $ l
                basiclineoption = unwords arg3s
                example =
                    (if differentiate then C2 . fromSmallWord8toInt8Diff else C1)
                        . read3
                            (not . L.null . filter (not . isSpace))
                            1.0
                            (mconcat . h . createSyllablesPL writingSystem allophones arrCharClassification segmentRules basicSpaces additionalDelims)
                        . unwords $
                        arg3s
                le = doubleFunc (length :: [Word8] -> Int) (length :: [Int8] -> Int) example
                lb = doubleFunc (length :: [Word8] -> Int) (length :: [Int8] -> Int) basecomp
                gcd1 = gcd le lb
                ldc = le * lb `quot` gcd1
                mulp = ldc `quot` lb
                --        max2 = maximum basecomp
                compards =
                    let ff g1 g2 ks =
                            if isWord8Based ks
                                then C1 . g1 . (\(C1 us) -> us) $ ks
                                else C2 . g2 . (\(C2 us) -> us) $ ks
                     in ff (concatMap (replicate mulp)) (concatMap (replicate mulp)) basecomp
                variants1 = force . uniquenessVariants2GNBL ' ' id id id perms $ ll
            return
                ( power10
                , ldc
                , compards
                , html
                , filtering
                , dcfile
                , selStr
                , prestr
                , poststr
                , lineNmb
                , emptyline
                , splitting
                , filesave
                , codesave
                , concurrently
                , basiclineoption
                , lineNumbersSel
                , variants1
                )

processingF ::
    -- | A function that specifies what 'Char's in the list the first argument makes to be the function sensitive to. Analogue of the @g@ function in the definition: https://hackage.haskell.org/package/phonetic-languages-simplified-examples-array-0.21.0.0/docs/src/Phonetic.Languages.Simplified.Array.Ukrainian.FuncRep2RelatedG2.html#parsey0Choice. Use just small 'Char' if they are letters, do not use \'.\' and spaces.
    (String -> String) ->
    -- | Data used to obtain the phonetic language representation of the text.
    GWritingSystemPRPLX ->
    -- | The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly.
    [(Char, Char)] ->
    CharPhoneticClassification ->
    SegmentRulesG ->
    -- | Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
    BasicSpaces ->
    -- | Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
    AdditionalDelimiters ->
    -- | See the conversion function 'Aftovolio.General.Datatype3.zippedDouble2Word8'. It is easier to obtain the function @f::[[[PhoneticPhenomenonRep]]]->[[Double]]@, and, afterwards, you can use 'zippedDouble2Word8' to transform the main semantic kernel of [(PhoneticPhenomenonRep, Double)] into [(PhoneticPhenomenonRep, Word8)]. For more information, see 'https://hackage.haskell.org/package/aftovolio-0.6.2.0/src/README.md' in the section 'Ability to use your own durations of representations of sounds or phonetic phenomena'.
    ([[[PhoneticPhenomenonRep]]] -> [[Word8]]) ->
    Int ->
    HashCorrections ->
    (Int8, [Int8]) ->
    [[String]] -> -- ^ Is intended to become a valid 'Concatenations' that are to be prepended to the next word.
    [[String]] -> -- ^ Is intended to become a valid 'Concatenations' that are to be appended to the previous word
    -- | The hashing function step. The default value is 20. Is expected to be greater than 2, and better greater than 12.
    Int ->
    String ->
    IO ()
processingF selFun writingSystem allophones arrCharClassification segmentRules basicSpaces additionalDelims h numTest hc (grps, mxms) ysss zsss hashStep xs =
    argsProcessing writingSystem allophones arrCharClassification segmentRules basicSpaces additionalDelims h ysss zsss xs >>= \( power10
                                                            , ldc
                                                            , compards
                                                            , html
                                                            , filtering
                                                            , dcfile
                                                            , selStr
                                                            , prestr
                                                            , poststr
                                                            , lineNmb
                                                            , emptyline
                                                            , splitting
                                                            , filesave
                                                            , codesave
                                                            , concurrently
                                                            , basiclineoption
                                                            , lineNumbersSel
                                                            , variants1
                                                            ) ->
        generalF
            power10
            ldc
            compards
            html
            filtering
            dcfile
            selStr
            selFun
            (prestr, poststr)
            lineNmb
            writingSystem
            allophones
            arrCharClassification
            segmentRules
            basicSpaces
            additionalDelims
            h
            numTest
            hc
            (grps, mxms)
            hashStep
            emptyline
            splitting
            (filesave, codesave)
            concurrently
            basiclineoption
            lineNumbersSel
            variants1
            >> return ()
{-# INLINE processingF #-}

{- | Specifies the group of the command line arguments for 'processingF', which specifies the
PhLADiPreLiO constraints. For more information, see:
https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#constraints
-}
cSpecs :: CLSpecifications
cSpecs = zip ["+a", "+b", "+l2", "+ln", "+nm"] . cycle $ [-1]

bSpecs :: CLSpecifications
bSpecs =
    [ ("+f", 2)
    , ("+m", 2)
    , ("+m2", 2)
    , ("+m3", 3)
    , ("+ul", 1)
    , ("+w", 1)
    , ("+dc", 2)
    , ("+q", 1)
    , ("-cm", 1)
    , ("+P", 1)
    ]

-- | 'selectSounds' converts the argument after \"+ul\" command line argument into a list of sound representations that is used for evaluation of \'uniqueness periods\' properties of the line. Is a modified Phonetic.Languages.Simplified.Array.General.FuncRep2RelatedG2.parsey0Choice from the @phonetic-languages-simplified-generalized-examples-array-0.19.0.1@ package.
selectSounds ::
    -- | A function that specifies what 'Char's in the list the first argument makes to be the function sensitive to. Analogue of the @g@ function in the definition: https://hackage.haskell.org/package/phonetic-languages-simplified-examples-array-0.21.0.0/docs/src/Phonetic.Languages.Simplified.Array.Ukrainian.FuncRep2RelatedG2.html#selectSounds. Use just small 'Char' if they are letters, do not use \'.\' and spaces.
    (String -> String) ->
    String ->
    String
selectSounds g xs =
    f . sortOn id . concatMap g . words . map (\c -> if c == '.' then ' ' else c) $
        us
  where
    (_, us) = break (== '.') . filter (\c -> c /= 'H' && c /= 'G') $ xs
    f (x : ts@(y : _))
        | x == y = f ts
        | otherwise = x : f ts
    f xs = xs

-- | Internal part of the 'generalF' for processment in case of using tests mode.
testsOutput ::
    (Show a1, Integral a1) =>
    Bool ->
    Int ->
    -- | Whether to filter out all groups of \'={digits}\' from the lines.
    Bool ->
    (Int -> Compards -> Int8 -> [Int8] -> String -> a1) ->
    Int ->
    Int ->
    [String] ->
    IO [String]
testsOutput concurrently syllN filtering f ldc numTest universalSet = do
    putStrLn "Feet   Val  Stat   Proxim"
    (if concurrently then mapConcurrently else mapM)
        ( \(q, qs) ->
            let m = stat1 syllN (q, qs)
                (min1, max1) = force . fromJust . minMax11By (comparing (f ldc (C1 []) q qs)) $ universalSet
                mx = f ldc (C1 []) q qs max1
                strTest =
                    ( show (fromEnum q)
                        `mappend` "   |   "
                        `mappend` show mx
                        `mappend` "     "
                        `mappend` show m
                        `mappend` "  -> "
                        `mappend` showFFloat (Just 3) (100 * fromIntegral mx / fromIntegral m) "%"
                        `mappend` ( if rem numTest 10 >= 4
                                        then 
                                            ( newLineEnding
                                                `mappend` (if filtering then removeChangesOfDurations else id) min1
                                                `mappend` newLineEnding
                                                `mappend` (if filtering then removeChangesOfDurations else id) max1
                                                `mappend` newLineEnding
                                            )
                                        else ""
                                  )
                    )
             in putStrLn strTest >> return strTest
        )
        . zip (sel2 numTest)
        $ (sel numTest)

-- | Internal part of the 'generalF' for processment with a file.
outputWithFile ::
    -- | See the conversion function 'Aftovolio.General.Datatype3.zippedDouble2Word8'. It is easier to obtain the function @f::[[[PhoneticPhenomenonRep]]]->[[Double]]@, and, afterwards, you can use 'zippedDouble2Word8' to transform the main semantic kernel of [(PhoneticPhenomenonRep, Double)] into [(PhoneticPhenomenonRep, Word8)]. For more information, see 'https://hackage.haskell.org/package/aftovolio-0.6.2.0/src/README.md' in the section 'Ability to use your own durations of representations of sounds or phonetic phenomena'.
    ([[[PhoneticPhenomenonRep]]] -> [[Word8]]) ->
    -- | Data used to obtain the phonetic language representation of the text.
    GWritingSystemPRPLX ->
    -- | The pairs of the 'Char' that corresponds to the similar phonetic languages consonant phenomenon (e. g. allophones). Must be sorted in the ascending order to be used correctly.
    [(Char, Char)] ->
    CharPhoneticClassification ->
    SegmentRulesG ->
    -- | Corresponds to the 100 delimiter in the @ukrainian-phonetics-basic-array@ package.
    BasicSpaces ->
    -- | Corresponds to the 101 delimiter in the @ukrainian-phonetics-basic-array@ package.
    AdditionalDelimiters ->
    -- | If not null than instead of rhythmicity evaluation using hashes and and feets, there is computed a diversity property for the specified 'String' here using the 'selectSounds' function. For more information, see: 'https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#types'
    String ->
    -- | A value that the different options are compared with. If no command line argument \"+di\" was added, then this is a `C1` applied to the list of positive values normed by 255 (the greatest of which is 255) that the line options are compared with. If null, then the program works without comparison. The length of it must be a least common multiplier of the (number of syllables plus number of \'_digits\' groups) to work correctly. Is not used when the next 'FilePath' and 'String' arguments are not null. If \"+di\" command line argument was  provided, then this corresponds to the case of differentiation.
    Compards ->
    [AftovolioGen] ->
    Int ->
    Int8 ->
    -- | A file to be probably added output parts to.
    FilePath ->
    Int ->
    IO ()
outputWithFile h writingSystem allophones arrCharClassification segmentRules basicSpaces additionalDelims selStr compards sRepresent code grps fs num
    | mBool && code >= 10 && code <= 19 && grps == 2 =
        putStrLn (mconcat [textP, newLineEnding, breaks, newLineEnding, show rs])
            >> appendF
                ( (if code >= 15 then mconcat [show rs, newLineEnding, breaks, newLineEnding] else "")
                    `mappend` outputS
                )
    | otherwise = appendF outputS
  where
    mBool =
        L.null selStr
            && doubleFunc (L.null :: [Word8] -> Bool) (L.null :: [Int8] -> Bool) compards
    appendF = appendFile fs
    lineOption = head . filter (\(S k _ _) -> k == num) $ sRepresent
    textP = (\(S _ _ ts) -> ts) lineOption
    outputS = outputSel lineOption code
    qqs =
        readEq4
            (mconcat . h . createSyllablesPL writingSystem allophones arrCharClassification segmentRules basicSpaces additionalDelims)
            (map (map charS) . mconcat . createSyllablesPL writingSystem allophones arrCharClassification segmentRules basicSpaces additionalDelims)
            . basicSplit $
            textP
    (breaks, rs) = showZerosFor2PeriodMusic qqs
