# Revision history for dobutokO2

## 0.1.0.0 -- 2020-03-04

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2020-03-05

* Second version. Added the functionality connected with octaves. Some documentation and code improvements.

## 0.3.0.0 -- 2020-03-06

* Third version. Added the opportunity to specify more parameters: additionally basic sound duration and the level of obertones influence the generated sounds.
Fixed issues with possibly incorrect argument sending to the SoX executable. Some documentation and code improvements.

## 0.3.1.0 -- 2020-03-06

* Third version revised A. Fixed issues with not being compiled because of the ambiguous variables and wrong function application.

## 0.3.1.1 -- 2020-03-06

* Third version revised B. Some documentation improvements for README.markdown file.

## 0.4.0.0 -- 2020-03-07

* Fourth version. Fixed issues with wrongly realized rhythm behaviour. Now it corresponds to the documented.

## 0.5.0.0 -- 2020-03-07

* Fifth version. Added the possibility to specify an existing file by its absolute path to be used as a source for the sound information for SoX to generate the
resulting file. Some documentation improvements.

## 0.5.1.0 -- 2020-03-07

* Fifth version revised A. Fixed issue with the order of actions in 'dobutokO2' function.

## 0.5.2.0 -- 2020-03-07

* Fifth version revised B. Fixed issue with the name of the recorded informational sound file in the 'recAndProcess' function.

## 0.5.3.0 -- 2020-03-07

* Fifth version revised C. Fixed issue with the noise reduction in the 'recAndProcess' function for the existing at the beginning file
(it is not done at all in such a case). 

## 0.6.1.0 -- 2020-03-08

* Sixth version revised A. Fixed issue with 'signsFromString' function that leads to performance penalty and to cycling. 

## 0.7.0.0 -- 2020-03-10

* Seventh version. Added new explicit dependency (mmsyn2) that earlier was implicit. Added the opportunity to define in how many times the amplitude
for the second lower note is greater than for the main one and to define the music intervals for the notes (if any). Some improvements for the documentation.

## 0.7.1.0 -- 2020-03-10

* Seventh version revised A. Fixed issue with being not compiled because of the implicit dependency being not explicitly specified.

## 0.7.1.1 -- 2020-03-10

* Seventh version revised B. Fixed issue with improper README.markdown file information.

## 0.7.2.0 -- 2020-03-11

* Seventh version revised C. Improved README.markdown file. Added exporting for all the basic functions in the DobutokO.Sound module.

## 0.7.2.1 -- 2020-03-11

* Seventh version revised D. Fixed issue with being not compiled because of the syntactic error.

## 0.8.0.0 -- 2020-03-12

* Eigth version. Added the new functions to the library that allow to produce richer sounds especially 'oberSoXSynth2FDN' one. The user can now specify a
function to be passed as an argument to it and obtain therefore own obertones formula. Added OPTIONS_GHC pragma to the module DobutokO.Sound.

## 0.8.0.1 -- 2020-03-12

* Eigth version revised A. Added the new functions to the exporting by the library.

## 0.8.1.0 -- 2020-03-12

* Eigth version revised B. Changed the sophisticated mechanism to provide real obertones in the 'oberSoXSynth2FDN' function.

## 0.8.2.0 -- 2020-03-12

* Eigth version revised C. Changed the sophisticated mechanism to provide real obertones in the 'oberSoXSynth2FDN' function. Fixed issues with some zeroes
in the functions. Some code improvements that give more stability. Documentation improvement.

## 0.9.0.0 -- 2020-03-13

* Ninth version. Changed the module structure to four modules. Added new functions in the DobutokO.Sound.Functional module to filter possible beats. Changed some
exports for DobutokO.Sound module.

## 0.9.1.0 -- 2020-03-14

* Ninth version revised A. Removed two functions from DobutokO.Sound.Functional module because of their inefficient work that leads to very long
(possibly infinitely) computation. 

## 0.10.0.0 -- 2020-03-14

* Tenth version. Added again the possibility to filter possible beats. Added new simple functions that are likely to work with previously prepared
Vector (Double, Double) that satisfies some constraints. Some documentation imrovement. Some performance optimizations.

## 0.10.1.0 -- 2020-03-14

* Tenth version revised A. Some documentation improvements. Some performance optimizations.

## 0.11.0.0 -- 2020-03-16

* Eleventh version. Added a second executable O2help that can be used to convert multiline Ukrainian text input to the needed String for the dobutokO2
program. Rewritten DobutokO.Sound.Executable module to provide more modularity. 

## 0.11.0.1 -- 2020-03-16

* Eleventh version revised A. Some documentation improvement of README.markdown file.

## 0.11.1.0 -- 2020-03-16

* Eleventh version revised B. Inserted the functionality of the second auxiliary executable O2help into the single one -- dobutokO2 with "0" first command
line option. Changed the information in the README.markdown file appropriately.

## 0.11.2.0 -- 2020-03-16

* Eleventh version revised C. Fixed issue with being not compiled because of not properly defined arguments for a function 'o2help'
in the DobutokO.Sound.Executable module.

## 0.12.0.0 -- 2020-03-17

* Twelfth version. Added a possibility to produce sound in the dobutokO2 function with only one input provided for the first command line option "7". Made
functions in the DobutokO.Sound.Executable module more modular so this grows a possibility to reuse them. Added new functions for the new functionality. Some
documentation improvements. Added as additional file "text.dat.txt" an example of the configuration for "7" first command line input. 

## 0.13.0.0 -- 2020-03-18

* Thirteenth version. Fixed issues connected with that SoX is by default for some distributions unable to work with a quantity of given files significantly more
than 1000. This led to that the program and functions could not work properly with some big amounts of data or sound data source files of some sizes. Now the
program and library work with them in a much more stable manner using temporary additional files. Some documentation, code and export improvements.

## 0.14.0.0 -- 2020-03-19

* Fourteenth version. Fixed issues connected with unneeded empty extensionless intermediate files in the current directory. Added the possibility to create
the resulting "end.wav" file and to clean the disk space from "result*" files by adding two new first command line options "8" and "80". Please, use the last
one with a special attention. See README.markdown for more information. Fixed issues with negative frequencies and durations possible in the
DobutokO.Sound.Functional functions. Added some more error messages to make debugging easier.

## 0.15.0.0 -- 2020-03-20

* Fifteenth version. Added an opportunity to work not only with octaves but also with n-th elements sets of consequential notes with the variants
of 2, 3, 4, 6, or 9 notes. Octave from such a perspective is a set of 12 notes. Some documentation improvements for the README.markdown file.

## 0.15.1.0 -- 2020-03-20

* Fifteenth version revised A. Fixed issue with the wrong mapping between the first command line arguments and the executable behaviour. Added the needed
behaviour for the "61" first command line option. Some improvement for the README.markdown file.

## 0.16.0.0 -- 2020-03-23

* Sixteenth version. Changed dependency version for directory package. Some documentation improvements for README.markdown file. Added a new module
DobutokO.Sound.IntermediateF to work with intermediate files. Changed the names of the some functions in DobutokO.Sound module and its exports.

## 0.17.0.0 -- 2020-03-25

* Seventeenth version. Added a few functions ('reverbE', 'reverb1E' etc.) to work with SoX effects to DobutokO.Sound.IntermediateF module. Added some additional
information in a dobutokO2.cabal file.

## 0.17.1.0 -- 2020-03-25

* Seventeenth version revised A. Fixed issues with being not compiled and duplicate export in 'DobutokO.Sound.IntermediateF'. Added two new functions
'soxE' and 'soxE1'. 

## 0.18.0.0 -- 2020-03-26

* Eighteenth version. Added three new functions to the DobutokO.Sound.IntermediateF module for recording and playing of the files.
Some documentation improvements.

## 0.19.0.0 -- 2020-03-27

* Nineteenth version. Added a new module DobutokO.Sound.ParseList to parse entered list of Int. Added the possibility to use "99" and "999" first command line
options that allows to play and edit with SoX effects applied respectively the sequence of the "result\*wav" files in the current directory. Added functions
to realize the functionality to different modules. Some documentation improvements.

## 0.19.1.0 -- 2020-03-28

* Nineteenth version revised A. Fixed issue with the infinite lists and thus the divergent functions. Changed some functions and fulfilled them with a limitation
parameter. Some minor documentation improvements.

## 0.20.0.0 -- 2020-03-28

* Twentieth version. Improved behaviour of the functions in the DobutokO.Sound.Functional module. Added new functions to produce melodies. Some minor
documentation improvements.

## 0.21.0.0 -- 2020-03-30

* Twenty-first version. Starting from the version 0.21.0.0 the package extends its library functions with the possibility to create not only
single notes or intervals of sounds playing simultaneously but also sets of three, four, five, six, seven or more sounds played simultanously
with their obertones. For more information, please, refer to the documentation for the DobutokO.Sound.Functional module. Added the needed functionality
for this. Rearrange modules DobutokO.Sound and DobutokO.Sound.Functional to provide more flexibility and shorten the code. Added new generalized functions
to the DobutokO.Sound.Functional module. Some documentation improvements.

## 0.22.0.0 -- 2020-04-02

* Twenty-second version. Added 1G generalized functions to the DobutokO.Sound.Functional module. They allow additional volume adjustment in dB given by
an additional vector of Double values for the overtones. Changed the names of the functions and documentation from 'ober' ('Ober') to English 'over'
('Over'). Fixed issue with 'partialTest_k' function for being not generated overtones files for the indices that can be without a remainder divided by
50. Some code and documentation improvements.

## 0.22.1.0 -- 2020-04-02

* Twenty-second version revised A. Fixed issues with being not compiled because of the syntaxis issues.

## 0.23.0.0 -- 2020-04-03

* Twenty-third version. Added 2G generalized functions to the DobutokO.Sound.Functional, and DobutokO.Sound.IntermediateF, and to DobutokO.Sound.Executable
modules. They allow to specify sound quality of the resulting files using additional parameter. Added also additional (including 1G) functions
to the same modules. Some code and documentation improvements. Added new command line options with "2" at the end.

## 0.24.0.0 -- 2020-04-04

* Twenty-fourth version. Fixed issue with being not compiled because of the unsupported operator (<$>) for GHC 7.8.4. Added possibilities to
work with DobutokO.Sound.Functional module function f more explicitly by three functions 'maybeFFromStrVec', 'fVecCoefs' and 'showFFromStrVec'. Provided
information about examples in the GitHub repository dobutokO2-examples. Some code and documentation improvements. 

## 0.24.1.0 -- 2020-04-04

* Twenty-fourth version revised A. Fixed issue with being not compiled because of the mapM_ applied for list and not vector for GHC 7.8.4.
Some code and documentation improvements. 

## 0.24.2.0 -- 2020-04-06

* Twenty-fourth version revised B. Fixed issue with overwritten "result.wav" file in some functions. Now the program works as expected for these functions. Some minor documentation improvements. 

## 0.24.3.0 -- 2020-04-06

* Twenty-fourth version revised C. Fixed issue with being not compiled code because of the wrong type signature. 

## 0.24.4.0 -- 2020-04-07

* Twenty-fourth version revised D. Fixed issues with segmentation faults in some functions. Added two new functions to the DobutokO.Sound.Functional
module. Improved stability.

## 0.24.5.0 -- 2020-04-07

* Twenty-fourth version revised E. Fixed issues with too silent sound for overtones. Imroved precision settings for functions so that it is expected
the generated sound to be more expressive. Improved stability. 

## 0.24.6.0 -- 2020-04-07

* Twenty-fourth version revised F. Fixed issue with being not compiled.

## 0.25.0.0 -- 2020-04-09

* Twenty-fifth version. Added more possibilities to edit function f from the DobutokO.Sound.Functional module. 

## 0.26.0.0 -- 2020-04-09

* Twenty-sixth version. Added more possibilities to edit function f from the DobutokO.Sound.Functional module. Now they include also gRem functions and
possibilities to work with two OvertonesO. Some code and documentation improvements in the DobutokO.Sound.Functional module. 

## 0.27.0.0 -- 2020-04-10

* Twenty-seventh version. Added more possibilities to edit intermediate sound files with silence and fading to avoid clipping while concatenating them
with SoX by default. Extended also the possibilities of the dobutokO2 executable with fading for "00" first command line option. Some documentation and
code improvements.

## 0.27.1.0 -- 2020-04-11

* Twenty-seventh version revised A. Fixed issue with functions 'freqsOverlapOvers' and 'elemsOverlapOvers' with wrongly defined for multiple situations.
Now they works properly. 

## 0.28.0.0 -- 2020-04-14

* Twenty-eigth version. Some code in existing modules generalization. Added a new explicit dependency -- bytestring package (earlier it was implicit 
dependency for the package dependencies). Added a new module DobutokO.Sound.Keyboard to deal with textual and possibly binary source of variativity. 
Some minor documentation improvements.

## 0.29.0.0 -- 2020-04-14

* Twenty-ninth version. Recent code in the DobutokO.Sound.Executable generalization. Added new functions for this. Added info about 
the YouTube list with some of the generated sounds as videos. Some minor documentation improvements.

## 0.30.0.0 -- 2020-04-15

* Thirtieth version. Recent code in the DobutokO.Sound.Executable generalization. Added a new function 'soundGen3G_O2G' for this. It allows very general 
approach to generation of the changing timbre for a solo sounds and their sequences. Some documentation improvements.

## 0.31.0.0 -- 2020-04-16

* Thirty-first version. Added a new functionality with splitting the 'OvertonesO' to the DobutokO.Sound.Functional module. 

## 0.32.0.0 -- 2020-04-17

* Thirty-second version. Made a way the greatest amplitudes are treated in the splitting functions for OvertonesO better defined. Added a new generalization for the 
splitting functionality to the DobutokO.Sound.Functional module. Some documentation improvements.

## 0.33.0.0 -- 2020-04-20

* Thirty-third version. Changed some functions to reduce unneeded parameter functions and arguments in the DobutokO.Sound.Functional and DobutokO.Sound.Executable 
modules. Added new functions to change elements of the OvertonesO 'fChangeFElem' and 'fChangeFElems'. Some documentation improvements.

## 0.34.0.0 -- 2020-04-21

* Thirty-fourth version. Fixed issues with deprecated documentation for 0.33.0.0 version. Added new functions to the DobutokO.Sound.Keyboard 
module to provide more control over reading from the files and stdin. Some documentation improvements.

## 0.35.0.0 -- 2020-04-22

* Thirty-fifth version. Added generalized versions for splitting functions (library). Some code improvements (reducing duplication). Some documentation 
improvements. Some descriptive changes in the dobutokO2.cabal file.

## 0.35.1.0 -- 2020-04-22

* Thirty-fifth version revised A. Fixed issues with some wrongly defined parameters for the new MN functions. Some documentation improvements.

## 0.35.2.0 -- 2020-04-22

* Thirty-fifth version revised B. Fixed issues with some other wrongly defined parameters for the new MN functions.

## 0.36.0.0 -- 2020-04-25

* Thirty-sixth version. Changed the structure of imported modules: now DobutokO.Sound.Functional imports DobutokO.Sound.IntermediateF and not vice versa 
as has been earlier. Added new 4G functions and rewritten a lot of previous ones so that now they support generation not only the sounds but also pauses 
and you can specify your own durations. Added new type synonyms and functions to work with them. Added new 5G functions to work with Intervals 
and 6G functions to work with Strengths (volume levels). Some documentation and code improvements.

## 0.36.1.0 -- 2020-04-25

* Thirty-sixth version revised A. Fixed issues with being not compiled for GHC 7.8.4 because of inappropriate length usage for vectors in 
DobutokO.Sound.Functonal module.

## 0.36.2.0 -- 2020-04-25

* Thirty-sixth version revised B. Fixed issues with being not compiled for GHC 7.8.4 because of inappropriate some functions usage in 
DobutokO.Sound.Functonal module.

## 0.36.3.0 -- 2020-04-26

* Thirty-sixth version revised C. Fixed issues with zero durations for SoX "synth" effect. Some documentation improvements.

## 0.37.0.0 -- 2020-04-28

* Thirty-seventh version. Added new functions to work with dB and corresponding type synonyms StrengthsDb and Strengths to DobutokO.Sound.Functional 
module. Fixed issues with being filtered to empty vector for durations. Added generalized versions for fading functions in the 
DobutokO.Sound.IntermediateF module. Added the "002" option to the executable and respective functions to DobutokO.Sound.Executable module. 
Added "united" variants for the '6GS' functions (where the 'Strengths' and 'Durations' are obtained from the same Ukrainian text). 
Some documentation improvements.

## 0.37.1.0 -- 2020-04-28

* Thirty-seventh version revised A. Fixed issues with the negative argument for the SoX "synth" effect in the DobutokO.Sound module.

## 0.38.0.0 -- 2020-05-02

* Thirty-eigth version. Added new datatype Params to DobutokO.Sound.Functional module. Added new generalized functions to work with Params to 
DobutokO.Sound.Functional and DobutokO.Sound modules. They allow to use tonalities in compositions. Although a great part of the already existed 
functions in the modules provides very similar behaviour to the some variant of the new ones they still are left in the modules.

## 0.38.1.0 -- 2020-05-04

* Thirty-eigth version revised A. Fixed issue with being not exported constructors in a Params datatype. 

## 0.39.0.0 -- 2020-05-07

* Thirty-ninth version. Added new functions with Params to DobutokO.Sound.Executable module. Changed the structure of the modules DobutokO.Sound.Functional 
and DobutokO.Sound.Executable into more logical one. Added three new functions for special SoX effects to the DobutokO.Sound.IntermediateF module. 
Some minor documentation improvements.

## 0.39.0.1 -- 2020-05-08

* Thirty-ninth version revised A. Some documentation improvements.

## 0.39.1.0 -- 2020-05-14

* Thirty-ninth version revised B. Changed bounds for the dependencies so that now also GHC 8.10* series are supported. Added new predicates to work with Params 
to DobutokO.Sound.Functional module for better access to inner structure of it. Changed also a volume and durations of the sounds taken from 
the loudness and durations respectively of the Ukrainian sounds representations in the mmsyn6ukr package accordingly to that ones of the new. 
Some documentation improvements. 

## 0.40.0.0 -- 2020-05-19

* Fourtieth version. Added mmsyn7l as a dependency. Moved some functions from DobutokO.Sound.IntermediateF module to MMSyn7l module. This 
allows to use their functionality without necessary installation of the dobutokO2 package (only mmsyn7* series using).

## 0.41.0.0 -- 2020-05-20

* Fourty-first version. Completely changed the modules structure added new ones and rearranged the existing, module DobutokO.Sound splitted into three 
different modules: DobutokO.Sound.Overtones, DobutokO.Sound.Uniq and DobutokO.Sound.Octaves. This was done to simplify compilation and to produce more 
logical and semantic structure. The most complicated functions from DobutokO.Sound.Functional module are moved to a new package dobutokO3. Some code 
improvements.

## 0.42.0.0 -- 2020-06-24

* Fourty-second version. Changed Double to Float in the modules where possible to reduce redundancy and to open access to new possibilities. The shift is inspired 
by: https://www.youtube.com/watch?v=FYTZkE5BZ-0
Some minor code and documentation improvements.

## 0.43.0.0 -- 2020-08-16

* Fourty-third version. Added a new dependecy uniqueness-periods with more efficient calculations of the uniquenessPeriods function. Changed the dependencies boundaries. 

## 0.44.0.0 -- 2024-12-19

* Fourty-fourth version. Added github repository and bug reporting. Updated the dependencies, so that now there is aftovolio included. Some minor code improvements.

## 0.44.1.0 -- 2024-12-20

* Fourty-fourth version revised A. Fixed some issues with ported code.

## 0.45.0.0 -- 2025-02-16

* Fourty-fifth version. Added new module DobutokO.Sound.Aftovolio.Ukrainian.Filter with the function that can be used for creation of the rhythmic patterns from the Ukrainian text.

## 0.45.1.0 -- 2025-02-16

* Fourty-fifth version revised A. Fixed issue with empty export of the module DobutokO.Sound.Aftovolio.Ukrainian.Filter.

 ## 0.45.1.1 -- 2025-02-16

* Fourty-fifth version revised B. Fixed issues with documentation for the module DobutokO.Sound.Aftovolio.Ukrainian.Filter and case of empty last argument.

## 0.45.2.0 -- 2025-02-24

* Fourty-fifth version revised C. Updated the dependency boundaries. Some documentations improvements.

## 0.45.3.0 -- 2025-05-28

* Fourty-fifth version revised D. Updated the dependency boundaries.
