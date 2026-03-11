# Revision history for aftovolio

## 0.1.0.0 -- 2024-09-21

* First version. Released on an unsuspecting world.

##  0.2.0.0 -- 2024-09-22

* Second version. Added possibility to specify sequence of Word8 numbers as durations to  be compared with in the +ln ... -ln command line arguments group.

##  0.2.1.0 -- 2024-09-28

* Second version revised A. Updated dependencies. Some minor code usability improvements.
 
##  0.3.0.0 -- 2024-10-13

* Third version, Added a new possibility to compare lines using the "+di" command line argument. If present implies the "differentiation" mode of computation for the comparing options with the line in +l2 or +ln groups of command line arguments. Is useful mostly in case of the line to compare with has approximately the same number of syllables as the option lines.

##  0.3.0.1 -- 2024-10-15

* Third version revised A. Fixed some issues with documentation README.md file.

## 0.3.0.2 -- 2024-10-20

* Third version revised B. Some minor code improvements. Added 'Peculiarities of the stressed and unstressed syllables in Ukrainian and other languages' section to the README.md file.

## 0.4.0.0 -- 2024-10-22

* Fourth version. Added new command line parameter "-e" to control whether to print groups of "={digits}" on the screen while making output. Changed the number of arguments in various functions. 

## 0.5.0.0 -- 2024-11-18

* Fifth version. Removed +n command line parameter and the descending sorting of the resulting data. Changed the printing behaviour so that there is no longer too long half of the columns as empty one printed. Some code simplification in the module Aftovolio.Halfsplit. Added new constraint type G for specifying that more than two elements should be groupped together as a one whole (with possible inner permutations inside the group) during all the permutations.

## 0.5.1.0 -- 2024-11-18

* Fifth version revised A. Fixed issue with eternal loop in the halfsplit1G function.

## 0.5.1.0 -- 2024-11-19

* Fifth version revised B. Fixed documentation to reveal the latest changes.

## 0.6.0.0 -- 2024-11-21

* Sixth version. Added new module Aftovolio.PermutationsArrMini2. Added new command line argument "+P <non-negative Int, presumably in the range 0..3>", which specifies the general type of permutations of the words and their concatenations. +P 0 corresponds the full set of all possible permutations (the default behaviour, also in case of no specification at all), +P 1 corresponds to the set of permutations, where just one word can change its position (the elementary, the least possible permutation), +P 2 corresponds to the set of permutations, where two words can be swapped one with another, +P 3 corresponds to the set of permutations, where no more than two words can change their positions, including the cases of no changes at all and just one word changes its position. All, except +P 0 provide less permutations in general and are quicker to be computed and displayed. This one is more useful in case of extended sets of words, e. g. when there are 8 or 9 words in the line. Fixed issue with genPermutations function. Some minor code and documentation improvements.

## 0.6.1.0 -- 2024-11-29

* Sixth version revised A. Fixed some issues with the documentation.

## 0.6.2.0 -- 2024-11-30

* Sixth version revised B. Improved documentation and code readability for several modules.

## 0.7.0.0 -- 2025-02-15

* Seventh version. Added new command line argument group +nm ... -nm and changed the arity of the main functions. These arguments are the line numbers of the sorted Aftovolio data to be displayed in the modes except tests and file appending.

## 0.7.1.0 -- 2025-02-24

* Seventh version revised A. Changed the name of the one of the Main.hs files so that to avoid duplications on Stackage. Added possibility to filter out the music information in the text in the music mode using "-m" command line argument for unconcatUkr executable.

## 0.8.0.0 -- 2025-05-28

* Eighth version. Fixed issue with printing of the output for many cases related to formatting of the lines for the second column. Refactored Aftovolio.Halflsplit module, added new funcions to ease reading and testing of the code. Changed the behaviour of -e command line option, so that now it suppresses printing all music mode notation in the form of '={digits}' or '_{digits}'. 
