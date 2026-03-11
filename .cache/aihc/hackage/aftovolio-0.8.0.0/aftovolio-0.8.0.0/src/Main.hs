{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}

{-# OPTIONS_GHC -rtsopts -threaded #-}

module Main where

import GHC.Base
import GHC.Num (abs,(+),(-),(*))
import Text.Read (readMaybe)
import Text.Show (show) 
import System.IO (putStrLn,readFile, hSetNewlineMode, stdout, universalNewlineMode)
import qualified Rhythmicity.MarkerSeqs as R hiding (id) 
import Data.List hiding (foldr)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe,catMaybes) 
import Data.Tuple (snd)
import Aftovolio.Ukrainian.PrepareText 
import System.Environment (getArgs)
import GHC.Int (Int8)
import GHC.Word (Word8) 
import CLI.Arguments
import CLI.Arguments.Get
import CLI.Arguments.Parsing
import Aftovolio.ConstraintsEncoded
import Aftovolio.StrictVG
import Aftovolio.Ukrainian.IO
import Aftovolio.General.Datatype3 (read3)
import Aftovolio.General.Distance
import GHC.Real ((/),quot,gcd)
import Aftovolio.Halfsplit (readNums)
import Aftovolio.Ukrainian.Syllable 
import Aftovolio.Ukrainian.SyllableWord8
import Aftovolio.Ukrainian.ReadDurations
import Data.ChooseLine2
import Control.DeepSeq
import Aftovolio.PermutationsRepresent

main :: IO ()
main = do
  args0 <- getArgs
  let (argCBs, args) = parseHelp args0 -- Used for +b ... -b parsing that is done avoiding general scheme of cSpecs.
      (argsA, argsB, argsC, arg2s) = args2Args31R ('+','-') (aSpecs `mappend` bSpecs `mappend` cSpecs) args
      compareByLinesFinalFile = concat . getB "-cm" $ argsB
  if not . null $ compareByLinesFinalFile then do
    compareFilesToOneCommon 14 arg2s compareByLinesFinalFile
  else do
    let fileDu = concat . getB "+d" $ argsB
        sylD = let k = snd (fromMaybe 2 (readMaybe (concat . getB "+s" $ argsB)::Maybe Int) `quotRemInt` 4) in if k == 0 then 4 else k
    syllableDurationsDs <- readSyllableDurations fileDu
    let maxNumWords = let k = fromMaybe 7 (readMaybe (concat . getB "+x" $ argsB)::Maybe Int) in if k == 0 || abs k >= 9 then 9 else max (abs k) 2
        hc = R.readHashCorrections . concat . getB "+c" $ argsB
        dcspecs = getB "+dc" argsB
        power10' = fromMaybe 0 (readMaybe (concat . getB "+q" $ argsB)::Maybe Int)
        power10 
           | power10' < 0 && power10' > 4 = 0
           | otherwise = power10'
        (html,dcfile) 
          | null dcspecs = (False, "")
          | otherwise = (head dcspecs == "1",last dcspecs)
        prepare = oneA "-p" argsA
        selStr = concat . getB "+ul" $ argsB
        grpp = R.grouppingR . concat . getB "+r" $ argsB
        splitting = fromMaybe 54 (readMaybe (concat . getB "+w" $ argsB)::Maybe Int8)
        emptyline = oneA "+l" argsA
        numTest = fromMaybe 1 (readMaybe (concat . getB "-t" $ argsB)::Maybe Int)
        hashStep = fromMaybe 20 (readMaybe (concat . getB "+k" $ argsB)::Maybe Int)
        helpMessage = any (== "-h") args0
        helpArg = concat . getB "-h" $ argsB
        filedata = getB "+f" argsB
        concurrently = oneA "-C" argsA
        filtering = oneA "-e" argsA
        (multiline2, multiline2LineNum) 
          | oneB "+m3" argsB = 
              let r1ss = getB "+m3" argsB in 
                    if length r1ss == 3 
                        then let (kss,qss) = splitAt 2 r1ss in 
                                     (kss, max 1 (fromMaybe 1 (readMaybe (concat qss)::Maybe Int))) 
                        else (r1ss, 1)
          | oneB "+m2" argsB = (getB "+m" argsB,  max 1 (fromMaybe 1 (readMaybe (concat . getB "+m2" $ argsB)::Maybe Int)))
          | otherwise = (getB "+m" argsB, -1)
        (fileread,lineNmb)
          | null multiline2 = ("",-1)
          | length multiline2 == 2 = (head multiline2, fromMaybe 1 (readMaybe (last multiline2)::Maybe Int))
          | otherwise = (head multiline2, 1)
    (arg3s,prestr,poststr,linecomp3) <- do 
         if lineNmb /= -1 then do
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
             return $ (words lineF, line_1F,line1F,linecomp3)
         else return (arg2s, [], [],[])
    let differentiate = oneA "+di" argsA
        line2comparewith 
          | oneC "+l2" argsC || null linecomp3 = unwords . getC "+l2" $ argsC
          | otherwise = linecomp3
        basecomp 
          | oneC "+ln" argsC = (if differentiate then C2 . fromSmallWord8toInt8Diff else C1) . catMaybes . map (\xs -> readMaybe xs::Maybe Word8) . getC "+ln" $ argsC 
          | otherwise = (if differentiate then C2 . fromSmallWord8toInt8Diff else C1) . read3 (not . null . filter (not . isSpace)) 1.0 (mconcat . (if null fileDu then case sylD of { 1 -> syllableDurationsD; 2 -> syllableDurationsD2; 3 -> syllableDurationsD3; 4 -> syllableDurationsD4} 
                           else  if length syllableDurationsDs >= sylD then syllableDurationsDs !! (sylD - 1) else syllableDurationsD2) . createSyllablesUkrS) $ line2comparewith
 
        example = (if differentiate then C2 . fromSmallWord8toInt8Diff else C1) . read3 (not . null . filter (not . isSpace)) 1.0 (mconcat . (if null fileDu then case sylD of { 1 -> syllableDurationsD; 2 -> syllableDurationsD2; 3 -> syllableDurationsD3; 4 -> syllableDurationsD4} 
                           else  if length syllableDurationsDs >= sylD then syllableDurationsDs !! (sylD - 1) else syllableDurationsD2) . createSyllablesUkrS) $ (unwords arg3s)
        le = doubleFunc (length::[Word8]->Int) (length::[Int8]-> Int) example
        lb = doubleFunc (length::[Word8]->Int) (length::[Int8]-> Int) basecomp
        gcd1 = gcd le lb
        ldc = (le * lb) `quot` gcd1
        mulp = ldc `quot` lb
        compards = let ff g1 g2 ks = if isWord8Based ks then C1 . g1 . (\(C1 us) -> us) $ ks else  C2 . g2 . (\(C2 us) -> us) $ ks in ff (concatMap (replicate mulp)) (concatMap (replicate mulp)) basecomp
        (filesave,codesave)  
          | null filedata = ("",-1)
          | length filedata == 2 = (head filedata, fromMaybe 0 (readMaybe (last filedata)::Maybe Int))
          | otherwise = (head filedata,0)
        ll = take maxNumWords . (if prepare then id else words . mconcat . prepareTextN3 maxNumWords . unwords) $ arg3s
        l = length ll
        lineNumbersSel = readNums . getC "+nm" $ argsC
        argCs = catMaybes (fmap (readMaybeECG l) . getC "+a" $ argsC)
        permutationsType = bTransform2Perms . getB "+P" $ argsB
        !perms 
          | not (null argCBs) = filterGeneralConv l argCBs . permChoose permutationsType $ l
          | null argCs = permChoose permutationsType l
          | otherwise = decodeLConstraints argCs . permChoose permutationsType $ l
        variants1 = force . uniquenessVariants2GNBL ' ' id id id perms $ ll
    if helpMessage then do 
      hSetNewlineMode stdout universalNewlineMode
      helpPrint helpArg
    else generalF power10 ldc compards html filtering dcfile selStr (prestr, poststr) lineNmb fileDu numTest hc grpp sylD hashStep emptyline splitting (filesave, codesave) concurrently (unwords arg3s) lineNumbersSel variants1 >> return ()


bSpecs :: CLSpecifications
bSpecs = (zip ["+c","+d","+k","-h","+r","+s","-t","+ul","+w","+x","+q","+m2","-cm","+P"] . cycle $ [1]) `mappend` [("+f",2),("+m",2),("+dc",2),("+m3",3)] 

aSpecs :: CLSpecifications
aSpecs = zip ["+di", "-e", "+l", "-p", "-C"] . cycle $ [0]

cSpecs :: CLSpecifications
cSpecs = [("+a",-1),("+l2",-1),("+ln",-1),("+nm",-1)]

helpPrint :: String -> IO ()
helpPrint xs
  | xs == "0" = putStrLn "SYNOPSIS:\n"
  | xs == "1" = putStrLn "aftovolioUkr [[+a <AFTOVolio constraints> -a] [+b <extended algebraic AFTOVolio constraints> -b] [+P <non-negative Int>] [+c <HashCorrections encoded>] [-e] [+l] [+d <FilePath to file with durations>] [+k <number - hash step>] [+r <groupping info>] [+s <syllable durations function number>] [-p] [+w <splitting parameter>] [+f <FilePath to the file to be appended the resulting String> <control parameter for output parts>] [+x <maximum number of words taken>] [+nm <numbers of the lines in the sorted AftovolioUkr list to be displayed> -nm] [+dc <whether to print <br> tag at the end of each line for two-column output> <FilePath to the file where the two-column output will be written in addition to stdout>]] <Ukrainian textual line>\n" 
  | xs == "2" = putStrLn "aftovolioUkr [[+a <AFTOVolio constraints> -a] [+b <extended algebraic AFTOVolio constraints> -b] [+P <non-negative Int>] [+c <HashCorrections encoded>] [-e] [+l] [+d <FilePath to file with durations>] [+k <number - hash step>] [-t <number of the test or its absence if 1 is here> [-C +RTS -N -RTS]] [+s <syllable durations function number>] [-p] [+w <splitting parameter>] [+x <maximum number of words taken>]] <Ukrainian textual line>\n"
  | xs == "3" = putStrLn "aftovolioUkr [[+a <AFTOVolio constraints> -a] [+b <extended algebraic AFTOVolio constraints> -b] [+P <non-negative Int>] [+ul <diversity property encoding string>] [+l] [-p] [+w <splitting parameter>] [+f <FilePath to the file to be appended the resulting String> <control parameter for output parts>] [+x <maximum number of words taken>] [+dc <whether to print <br> tag at the end of each line for two-column output> <FilePath to the file where the two-column output will be written in addition to stdout>]] <Ukrainian textual line>\n"
  | xs == "4" = putStrLn "aftovolioUkr [[+a <AFTOVolio constraints> -a] [+b <extended algebraic AFTOVolio constraints> -b] [+P <non-negative Int>] [-e] [+l] [+d <FilePath to file with durations>] [+s <syllable durations function number>] [-p] [+w <splitting parameter>] [+q <power of 10 for multiplier in [2..6]>] [+f <FilePath to the file to be appended the resulting String> <control parameter for output parts>] [+x <maximum number of words taken>] [+nm <numbers of the lines in the sorted AftovolioUkr list to be displayed> -nm] [+dc <whether to print <br> tag at the end of each line for two-column output> <FilePath to the file where the two-column output will be written in addition to stdout>] [+l2 <a Ukrainian text line to compare similarity with> -l2]] [+di] <Ukrainian textual line>\n"
  | xs == "5" = putStrLn "aftovolioUkr [[+a <AFTOVolio constraints> -a] [+b <extended algebraic AFTOVolio constraints> -b] [+P <non-negative Int>] [-e] [+l] [+d <FilePath to file with durations>] [+s <syllable durations function number>] [-p] [+w <splitting parameter>] [+q <power of 10 for multiplier in [2..6]>] [+f <FilePath to the file to be appended the resulting String> <control parameter for output parts>] [+x <maximum number of words taken>] [+nm <numbers of the lines in the sorted AftovolioUkr list to be displayed> -nm] [+dc <whether to print <br> tag at the end of each line for two-column output> <FilePath to the file where the two-column output will be written in addition to stdout>] [+di] [+m <FilePath> <num1> +m2 <num2>]]\n"
  | xs == "6" = putStrLn "aftovolioUkr [[+a <AFTOVolio constraints> -a] [+b <extended algebraic AFTOVolio constraints> -b] [+P <non-negative Int>] [-e] [+l] [+d <FilePath to file with durations>] [+s <syllable durations function number>] [-p] [+w <splitting parameter>] [+q <power of 10 for multiplier in [2..6]>] [+f <FilePath to the file to be appended the resulting String> <control parameter for output parts>] [+x <maximum number of words taken>] [+nm <numbers of the lines in the sorted AftovolioUkr list to be displayed> -nm] [+dc <whether to print <br> tag at the end of each line for two-column output> <FilePath to the file where the two-column output will be written in addition to stdout>] [+di] [+m3 <FilePath> <num1> <num2>]]\n"
  | xs == "7" = putStrLn "aftovolioUkr [-cm <FilePath to write the resulting combined output to> <FilePaths of the files to be compared and chosen the resulting options line-by-line>]\n"
  | xs == "8" = putStrLn "aftovolioUkr [[+a <AFTOVolio constraints> -a] [+b <extended algebraic AFTOVolio constraints> -b] [+P <non-negative Int>] [-e] [+l] [+d <FilePath to file with durations>] [+s <syllable durations function number>] [-p] [+w <splitting parameter>] [+q <power of 10 for multiplier in [2..6]>] [+f <FilePath to the file to be appended the resulting String> <control parameter for output parts>] [+x <maximum number of words taken>] [+nm <numbers of the lines in the sorted AftovolioUkr list to be displayed> -nm] [+dc <whether to print <br> tag at the end of each line for two-column output> <FilePath to the file where the two-column output will be written in addition to stdout>] [+ln <a sequence of Word8 positive integer values not greater than 255 e. g. 24 54 57 159 45 39 to compare similarity with> -ln]][+di] <Ukrainian textual line>\n"
  | xs == "OR" = putStrLn "OR:"
  | xs == "l" = putStrLn "+l \t— if specified then the output for one property (no tests) contains empty lines between the groups of the line option with the same value of property. \n"
  | xs == "w" = putStrLn "+w \t— if specified with the next Int8 number then the splitting of the output for non-testing options is used. Is used when no \"-t\" argument is given. The output is split into two columns to improve overall experience. The parameter after the \"+w\" is divided by 10 (-10 for negative numbers) to obtain the quotient and remainder (Int8 numbers). The quotient specifies the number of spaces or tabular characters to be used between columns (if the parameter is positive then the spaces are used, otherwise tabular characters). The remainder specifies the option of displaying. If the absolute value of the remainder (the last digit of the parameter) is 1 then the output in the second column is reversed; if it is in the range [2..5] then the output is groupped by the estimation values: if it is 2 then the first column is reversed; if it is 3 then the second column is reversed; if it is 4 then like 2 but additionally the empty line is added between the groups; if it is 5 then like for 3 and additionally the empty line is added between the groups. Otherwise, the second column is reversed. The rules are rather complex, but you can give a try to any number (Int8, [129..128] in the fullscreen terminal). The default value is 54 that corresponds to some reasonable layout.\n"
  | xs == "s" = putStrLn "+s \t— the next is the digit from 1 to 4 included. The default one is 2. Influences the result in the case of +d parameter is not given. \n"
  | xs == "d" = putStrLn "+d \t— if present, then afterwards should be a FilePath to the file with new durations of the Ukrainian AFTOVolio representations. They can be obtained using the information from the section on durations in the REEDME.md file by the following link: \nhttps://hackage.haskell.org/package/aftovolio-0.8.0.0/src/README.md\n"
  | xs == "p" = putStrLn "-p \t— if present the minimal grammar transformations (appending and prepending the dependent parts) are not applied. Can be useful also if the text is analyzed as a Ukrainian transcription of text in some other language.\n"
  | xs == "f" = putStrLn "+f \t— if present with two arguments specifies the file to which the output information should be appended and the mode of appending (which parts to write). The default value if the secodnd parameter is 0 or not element of [1,2,3,4,10,11,12,13,14,15,16,17,18,19] is just the resulting String option. If the second parameter is 1 then the sequential number and the text are written; if it is 2 then the estimation value and the string are written; if it is 3 then the full information is written i. e. number, string and estimation; if it is 4 then the number and estimation (no string).\nThe second arguments greater or equal to 10 take effect only if the meter consists of two syllables (in case of \"+r 21\" command line options given). If it is 10 in such a case then before appending the line option itself to the given file there is hashes information for this line option displayed. If it is 11 — the same hashes information is displayed before processing as for 1. If it is 12 — the same hashes information is displayed before processing as for 2 and so on for 13, 14. If it is 15 up to 19 — it is analogically to the the 10–14 but the hashes information is not only printed on the screen, but appended to the file specified. These values are intended to test the interesting hypothesis about where the pauses can occur. For more information on the hypothesis, see: \n https://www.academia.edu/105067761/Why_some_lines_are_easy_to_pronounce_and_others_are_not_or_prosodic_unpredictability_as_a_characteristic_of_text\n"
  | xs == "nm" = putStrLn "+nm ... -nm \t— if specified in the modes without tests and file single line output changes the output so that just the lines with the specified Int numbers are displayed in the order of the specified numbers. Please, delimit the numbers with spaces. To specify some range, use just dash as here: \'34-250\' meaning that lines with numbers between 34 and 250 inclusively will be displayed. Music mode additional information is displayed as well here.\n"
  | xs == "dc" = putStrLn "+dc \t— if specified with two further arguments then the first one can be 1 or something  else. If it is 1 then additionally to every line as usual there is printed also <br> html tag at the end of the line for the two-columns output. Otherwise, nothing is added to each line. The second argument further is a FilePath to the writable existing file or to the new file that will be located in the writable by the user directory. The two-column output will be additionally written to this file if it is possible, otherwise the program will end with an exception.\n"
  | xs == "a" = putStrLn "+a ... -a \t— if present contains a group of constraints for AFTOVolio. For more information, see: \nhttps://hackage.haskell.org/package/aftovolio-0.8.0.0/src/README.md in English or in Ukrainian: \nhttps://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Ukr.21.html#%D0%BE%D0%B1%D0%BC%D0%B5%D0%B6%D0%B5%D0%BD%D0%BD%D1%8F-constraints\n"
  | xs == "b" = putStrLn "+b ... -b \t— if present takes precedence over those ones in the +a ... -a group (the latter ones have no effect). A group of constraints for AFTOVolio using some boolean-based algebra. If you use parentheses there, please, use quotation of the whole expression between the +b and -b (otherwise there will be issues with the shell or command line interpreter related to parentheses). For example, on Linux bash or Windows PowerShell: +b \'P45(A345 B32)\' -b. If you use another command line environment or interpreter, please, refer to the documentation for your case about the quotation and quotes. For more information, see: \nhttps://hackage.haskell.org/package/aftovolio-0.8.0.0/src/README.md in English or: \nhttps://oleksandr-zhabenko.github.io/uk/rhythmicity/phladiprelioUkr.7.pdf in Ukrainian.\n"
  | xs == "P" = putStrLn "+P \t— if specified with the following non-negative Int from 0 to 3 including then specifies the general type of permutations of the words and their concatenations. +P 0 corresponds the full set of all possible permutations (the default behaviour, also in case of no specification at all), +P 1 corresponds to the set of permutations, where just one word can change its position (the elementary, the least possible permutation), +P 2 corresponds to the set of permutations, where two words can be swapped one with another, +P 3 corresponds to the set of permutations, where no more than two words can change their positions, including the cases of no changes at all and just one word changes its position. All, except +P 0 provide less permutations in general and are quicker to be computed and displayed. This one is more useful in case of extended sets of words, e. g. when there are 8 or 9 words in the line."
  | xs == "l2" = putStrLn "+l2 ... -l2 \t— if present and has inside Ukrainian text then the line options are compared with it using the idea of lists similarity. The greater values correspond to the less similar and more different lines. Has no effect with +dc group of command line arguments. Has precedence over +t, +r, +k, +c etc. groups of command line options so that these latter ones have no effect when +l2 ... -l2 is present.\n"
  | xs == "ln" = putStrLn "+ln ... -ln \t— if present and has inside a sequence of Word8 values i. e. positive integer numbers less than 256 then the line options are compared with it using the idea of lists similarity. The greater values correspond to the less similar and more different lines. Has no effect with +dc group of command line arguments. Has precedence over +t, +r, +k, +c etc. groups of command line options so that these latter ones have no effect when +ln ... -ln is present.\n"
  | xs == "di" = putStrLn "+di \t— if present implies the \"differentiation\" mode of computation for the comparing options with the line in +l2 or +ln groups of command line arguments. Is useful mostly in case of the line to compare with has approximately the same number of syllables as the option lines."
  | xs == "e" = putStrLn "-e \t— if present suppresses the printing of \"={digits}\" and \"_{digits}\" (the last one groups since the version 0.8.0.0) in the line option to the screen. Is used for better readability, often with \"+di\"."
  | xs == "q" = putStrLn "+q \t— if present with +l2 ... -l2 group of arguments then the next argument is a power of 10 which the distance between line option and the predefined line is quoted by. The default one is 0 (that means no change). You can specify not less than 0 and not greater than 4. Otherwise, these limit numbers are used instead. The greater value here leads to more groupped options output.\n"
  | xs == "ul" = putStrLn "+ul \t— afterwards there is a string that encodes which sounds are used for diversity property evaluation. If used, then +r group has no meaning and is not used. Unlike in the link, the argument \"1\" means computing the property for all the sound representations included (for all of the present representations, so the value is maximal between all other strings instead of \"1\"). For more information, see: https://oleksandr-zhabenko.github.io/uk/rhythmicity/PhLADiPreLiO.Eng.21.html#types\n"
  | xs == "r" = putStrLn "+r \t— afterwards are several unique digits not greater than 8 in the descending order — the first one is the length of the group of syllables to be considered as a period, the rest — positions of the maximums and minimums. Example: \"543\" means that the line is split into groups of 5 syllables starting from the beginning, then the positions of the most maximum (4 = 5 - 1) and the next (smaller) maximum (3 = 4 - 1). If there are no duplicated values then the lowest possible value here is 0, that corresponds to the lowest minimum. If there are duplicates then the lowest value here is the number of the groups of duplicates, e. g. in the sequence 1,6,3,3,4,4,5 that is one group there are two groups of duplicates — with 3 and 4 — and, therefore, the corresponding data after +r should be 7...2. The values less than the lowest minimum are neglected.\n"
  | xs == "c" = putStrLn "+c \t— see explanation at the link: https://hackage.haskell.org/package/rhythmic-sequences-0.8.0.0/docs/src/Rhythmicity.MarkerSeqs.html#HashCorrections Some preliminary tests show that these corrections influence the result but not drastically, they can lead to changes in groupping and order, but mostly leave the structure similar. This shows that the algorithms used are more stable for such changes.\n"
  | xs == "t" = putStrLn "-t \t— and afterwards the number in the range [0..179] (with some exceptions) showing the test for \'smoothness\' (to be more accurate - absence or presence of some irregularities that influences the prosody) to be run - you can see a list of possible values for the parameter here at the link: \nhttps://hackage.haskell.org/package/aftovolio-0.8.0.0/docs/src/Aftovolio.Tests.html#sel. \nFor ideas of actual usage of the tests, see the documentation by the links: https://www.academia.edu/105067723/%D0%A7%D0%BE%D0%BC%D1%83_%D0%B4%D0%B5%D1%8F%D0%BA%D1%96_%D1%80%D1%8F%D0%B4%D0%BA%D0%B8_%D0%BB%D0%B5%D0%B3%D0%BA%D0%BE_%D0%B2%D0%B8%D0%BC%D0%BE%D0%B2%D0%BB%D1%8F%D1%82%D0%B8_%D0%B0_%D1%96%D0%BD%D1%88%D1%96_%D0%BD%D1%96_%D0%B0%D0%B1%D0%BE_%D0%BF%D1%80%D0%BE%D1%81%D0%BE%D0%B4%D0%B8%D1%87%D0%BD%D0%B0_%D0%BD%D0%B5%D1%81%D0%BF%D1%80%D0%BE%D0%B3%D0%BD%D0%BE%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D1%96%D1%81%D1%82%D1%8C_%D1%8F%D0%BA_%D1%85%D0%B0%D1%80%D0%B0%D0%BA%D1%82%D0%B5%D1%80%D0%B8%D1%81%D1%82%D0%B8%D0%BA%D0%B0_%D1%82%D0%B5%D0%BA%D1%81%D1%82%D1%83?source=swp_share and https://hackage.haskell.org/package/aftovolio-0.8.0.0/src/README.md\n"
  | xs == "C" = putStrLn "-C \t— If specified with +RTS -N -RTS rtsoptions for the multicore computers with -t option then it can speed up the full computation of the all tests using more resources. Uses asynchcronous concurrent threads in computation. This groups (-C and rtsoptions) can be specified in any position as command line groups of arguments. The output is in this case not sorted in the order of 2,3,4,5,6,7, but can vary depending on your CPU configurations and states and / or some other factors.\n"
  | xs == "k" = putStrLn "+k \t— and then the number greater than 2 (better, greater than 12, the default value if not specified is 20). The greater value leads to greater numbers. The number less than some infimum here leads to wiping of some information from the result and, therefore, probably is not the desired behaviour. For most cases the default value is just enough sensible, but you can give it a try for other values.\n"
  | xs == "x" = putStrLn "+x \t— If specified with the further natural number then means that instead of maximum 7 words or their concatenations that can be analysed together as a line there will be taken the specified number here or 9 if the number is greater than 8. More words leads to more computations, therefore, please, use with this caution in mind. It can be useful mostly for the cases with the additional constraints specified (+a ... -a or +b ... -b groups, see above).\n"
  | xs == "m" = putStrLn "+m \t— If present followed with two arguments — the name of the file to be used for reading data for multiline processment and the second one — the number of line to be processed. The numeration of the lines in the file for the aftovolioUkr program here starts from 1 (so, it is: 1, 2, 3, 4,.. etc). The program then uses instead of text the specified line from the file specified here and prints information about its previous and next lines (if available).\n"
  | xs == "m2" || xs == "m3" = putStrLn "+m2 \t— If present, it means that the line with the corresponding number specified here will be taken from the same file specified as +m <file>. For example, the entry +m <file> 1 +m2 4 means that 1 line will be taken from the file <file> for the similarity analysis and it will be compared not with contents of +l2 ... -l2, but with the 4th line from the same file. \n\nThe abbreviation for +m <file> <num1> +m2 <num2> is +m3 <file> <num1> <num2>, which has the same meaning but a slightly shorter entry. \n\nIf <num1> == <num2>, then there is at least one of all the options with a property value of 0. \n\nYou can also use the \"music\" mode, which allows you to write better lyrics and music. To do this, you can add a record of the form _{set of digits} or ={set of digits} to a word or between syllables after the needed to be referred to, and this set of digits will be converted to a non-negative Double number by the program and then used to modulate the duration of the previous syllable or of the additional one(s) added here. The first digit in the record after '=' or '_' is a whole number and the rest is a fraction (mantissa). \n\nIf you have added _{set of digits} then this number will be multiplied by the duration of the syllable to which this insertion refers, and the resulting number will be rounded so that is can be no more than 255 (the maximum possible value of Word8 datatype that is used internally to calculate the statistics). Afterwards, this value is placed among the others in the place where this entry is inserted. \n\nIf you have added ={a set of digits} then this number will also be multiplied by the duration of the syllable to which this insertion refers, and the resulting number will be rounded and then will be placed instead of the duration that it was multiplied by (again, the maximum resulting value here can be 255, so that the greater values are rounded to this limit). This allows to make syllables longer or shorter to follow language or creative intentions and rules (e. g. in French there have been noticed that the duration of the last syllable in the word, especially in the sentence or phrase is prolonged to some extent, so you can use '=12' or '=13' or '=14'  here). Not as for the _{set of digits} group, you can place it only after the syllable and only once to change the duration of the syllable. If you use this option, the following _{set of digits} will be referred to the new changed duration here, so that \"так\"=2_05_1 will mean that you will use instead of the usual duration of the syllable \"так\" the twice as much duration, and then half of this twice as much (therefore, equal to duration of the syllable without altering) and then again the twice as much duration etc. The insertion of =1, =10, =100, =1000 etc will not change anything at all, except for the time of computations (you probably do not need this). \n\nThese two additional possibilities allows you to change rhythmic structures, and interpret this insert as a musical pause or, conversely, a musical phrase inserted to change the rhythm. What syllable does this insertion refer to? It refers to the previous syllable in the line. There can be several such inserts, their number is not limited by the program itself (in the program code), but a significant number of inserts can significantly increase the duration of the calculation. \n\nWhen the results of the program are displayed on the screen, these inserts will also be displayed in the appropriate places. \n\nIf you specify _1, _10, _100, _1000, etc. as such an insertion, the program will assume that this insertion duration is equal to the duration of the syllable to which it refers. If the set of digits is preceded by a 0, the insertion has a shorter duration, if the 1 in the first 4 examples above is followed by digits other than 0, or if another digit is used instead of 1 or 0, the insertion has a longer duration. For example, _05 means exactly half the duration of the syllable, and _2 means double the duration. In any case the resulting values cannot be greater than 255, the greater ones are rounded to 255.\n\nFor the \"music\" mode the number of syllables printed for the line does include the inserts.\n"
  | xs == "cm" = putStrLn "-cm \t— If present, the program works in a special comparative mode reading information from the several files line-by-line and prompting to choose the resulting option from the files given. If some files do not have such lines, then the resulting option for the file is empty. You choose the resulting option by typing its number on the terminal. The total number of sources is limited to no more than 14."
  | otherwise = helpFE xs
    where helpFE xs = mapM helpPrint js >> return ()
          js
            | xs == "-cm" = ["0","7","cm"] 
            | xs == "+l" = ["0","1","OR","2","OR","3","OR","4","OR","5","OR","6","OR","8","l"] 
            | xs == "+w" = ["0","1","OR","2","OR","3","OR","4","OR","5","OR","6","OR","8","w"] 
            | xs == "+s" = ["0","1","OR","2","OR","4","OR","5","OR","6","OR","8","s"] 
            | xs == "+d" = ["0","1","OR","2","OR","4","OR","5","OR","6","OR","8","d"] 
            | xs == "-p" = ["0","1","OR","2","OR","3","OR","4","OR","5","OR","6","OR","8","p"] 
            | xs == "+f" = ["0","1","OR","3","OR","4","OR","5","OR","6","OR","8","f"] 
            | xs == "+nm" = ["0","1","OR","4","OR","5","OR","6","OR","8","nm"]
            | xs == "+dc" = ["0","1","OR","3","OR","4","OR","5","OR","6","OR","8","dc"] 
            | xs == "+a" = ["0","1","OR","2","OR","3","OR","4","OR","5","OR","6","OR","8","a"] 
            | xs == "+b" = ["0","1","OR","2","OR","3","OR","4","OR","5","OR","6","OR","8","b"] 
            | xs == "+P" = ["0","1","OR","2","OR","3","OR","4","OR","5","OR","6","OR","8","P"] 
            | xs == "+l2" = ["0","4","l2"] 
            | xs == "+ln" = ["0","8","ln"] 
            | xs == "+di" = ["0","1", "OR", "2", "OR","4","OR","5","OR","6","OR","8","di"]
            | xs == "-e" = ["0","1","OR","2","OR","4","OR","5","OR","6","OR","8","e"]
            | xs == "+q" = ["0","4","OR","5","OR","6","OR","8","q"] 
            | xs == "+ul" = ["0","3","ul"] 
            | xs == "+r" = ["0","1","r"] 
            | xs == "+c" = ["0","1","OR","2","c"] 
            | xs == "-t" = ["0","2","t"] 
            | xs == "-C" = ["0","2","C"] 
            | xs == "+k" = ["0","1","OR","2","k"] 
            | xs == "+x" = ["0","1","OR","2","OR","3","OR","4","OR","5","OR","6","OR","8","x"] 
            | xs == "+m" = ["0","5","m"] 
            | xs == "+m2" = ["0","5","m2"] 
            | xs == "+m3" = ["0","6","m3"] 
            | otherwise = ["0","1","OR","2","OR","3","OR","4","OR","5","OR","6","OR","7","OR","8","l","w","s","d","p","f","nm","dc","a","b","P","l2","q","ul","r","c","t","C","k","x","m","m2","cm"]

