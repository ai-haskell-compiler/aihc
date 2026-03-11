Helps to create experimental music from a file (or its part) and a Ukrainian text. 
It can also generate a timbre for the notes. Uses SoX inside.


                ***** Usage *****
                =================

You can use it as a library or as an executable.

Please, check before executing whether there is no "x.wav", "test\*.wav",
"result\*.wav" and "end.wav" files in the current directory, because they
can be overwritten. The same can be said about "nx\*.wav" files in the directory.

For the executable you enter in the terminal:

dobutokO2 { 0 | 1 | 11 | 2 | 21 | 3 | 31 | 4 | 41 | 5 | 51 | 6 | 7 } {fileName} {Ukrainian text}

OR:

dobutokO2 { 8 | 80 | 9 | 99 | 999 }

OR:

dobutokO2 { 82 | 820 | 92 | 992 | 9992 } {volatile symbols together, e. g. "0"} {quality control, see information below}

OR:

dobutokO2 { 00 | 002 } {beginning symbols for the filenames "fade" effect to be applied to} {quality control, see information below}

  where filename is:
  the full name of the file to be recorded in the current directory or
  the full absolute path to the sound .wav file (or other one format
  that is supported by your SoX installation) to obtain sound information from.

If the first command line argument equals to one of the numbers below, then
the program behaves as follows:

  "1", or "3", or "5" ->  then the executable uses the overTones functions,
    so for the given parameters the overtones are the same for every call.

  "2", or "4", or "6" -> then the program uses uniqOverTones functions.

  "3", or "4", or "5", or "6" -> the program uses additional String to define
    signs for the harmonics coefficients for overtones.

  "11", or "21", or "31", or "41", or "51", or "61" -> the program works as for
     the respective inputs with only the first character in the option
       (e. g. for "51", this is "5"), but uses not octaves, but n-th elements sets
         of consequential notes consisting of 2, 3, 4, 6, or 9 elements (called
           'enky'). The usual octave is from such point of view a 12th elements set
             of consequential notes. This allows to create more 'condensed' and
               'narrower' compositions that being more defined can be at the
                 same time more precise.

  "9" -> the program works with existing "result\*.wav" files and you can replace
     some of them by other one(s) or their sequences. This allows to create files,
       then edit them using this first command line option (possibly for several
         times) and at last create a resulting melody file with "8" or "80" options.

  "99" -> the program takes a filename (it is a first command line argument and it is
     ignored) and the rest of the command line argument (they are treated as command
       line arguments to the SoX 'play' command). Then the program prompts you to
         specify the needed indeces for the "result\*.wav" files in the current
           directory (e. g. they can be obtained by executing the dobutokO2 with
             the first command line argument less than "8", or produced by some
               other means). You can specify multiple lists of Int to select the
                 needed files to be played with effects. Afterwards, the program
                   just plays these selected files applying the specified SoX
                     effects to them consequently. For example:
    dobutokO2 9 reverb -w
                       will play the selected (during execution) files with the
                         SoX "reverb -w" effect. For more information on effects,
                           please, refer to the SoX documentation.

  "999" -> similarly to "99", but instead of playing the files, SoX actually
      applies to them these effects and overwrites the files with the obtained ones.
        It is convenient way to test the sounding effects with firstly run the
          dobutokO2 with "99" first command line argument, and then, if suitable,
            with "999" one. Be aware, that after running with the latter one, the
              program cannot restore the files that were changed to their previous
                state, so be careful while running.
                           
  In more details:

  "0" -> the program just converts multiline Ukrainian text from stdin
     into a String and prints it to stdout. No other arguments are used.

  "00" -> the program applies "fade q 0.01 -0.0" SoX effect to every "zs\*.wav"
     (or instead of them to every "zs\*.flac") file in the current directory. 
       The 'zs' here is specified by the second command line argument.
        This allows to avoid clipping while just simply concatenating the files into
          one with SoX. The mentioned clipping can be desirable in some circumstances
            but generally is not necessary or is redundant. For more information,
              please, refer to the SoX documentation and to that one for 'fadeAllE'
                function in the DobutokO.Sound.IntermediateF module.
                
  "002" -> the program applies "fade q 0.002 -0.0" SoX effect to every "zs\*.wav"
     (or instead of them to every "zs\*.flac") file in the current directory. 
       Works similarly to the option "00" (see above).

  "1" -> basic functionality without the possibility to define individual overtones.

  "11" -> the same as "1", but works with enky. See general information above.
  
  "2" -> basic functionality with the possibility to define individual overtones.
     In such a case, another text gives the other overtones.

  "21" -> the same as "2", but works with enky. See general information above.

  "3" -> adittionally to basic functionality gives an opportunity 
     to specify the signs for the harmonics coefficients for overtones
       by additional String.

  "31" -> the same as "3", but works with enky. See general information above.

  "4" -> similarly to "2" gives an opportunity to specify the signs
     for the harmonics coefficients for overtones by additional String.

  "41" -> the same as "4", but works with enky. See general information above.

  "5" -> additionally to that one functionality provided by "3" gives
     an opportunity to specify in how many times the amplitude for
       the second lower note (if any) is greater, than the amplitude
         for the main note and specify the intervals to be used
           for every note.

  "51" -> the same as "5", but works with enky. See general information above.

  "6" -> the same as "5", but you can define also overtones by an additional String.

  "61" -> the same as "6", but works with enky. See general information above.

  "7" -> the program behaves like for the "5" option, but generates
     overtones using additional String and allows maximum control over
       the parameters. Besides, all the needed information it obtains from
         the singular formatted input, which can be ended with a keyboard keys
           combination that means an end of input (e. g. for Unices, that is
             probably Ctrl + D). '@' are separators for the input parts
               for their respective parts. For more information about the
                 format of the single input, see:
        https://drive.google.com/open?id=10Z_GRZR4TKoL5KXfqPm-t-4humuHN0O4
                  The file is also provided with the package as text.dat.txt.
                   The last two or three inputs (an input just here means
                     a textual input between two '@') can be omitted, the
                       program will work also but with less control for
                         the user possible.

   "8" -> the program just creates from input "result\*" files the "end.wav" by
     concatenating them into one. It is mostly useful after some processment
       on the "result\*" files after previous execution with other lesser first
         command line arguments to get the test final sound file. It can be then
           listened to and probably remade again by editing the "result\*" files
             and running the program with this option again. In such a case,
               none from the other command line arguments is important for the
                 program running, so they all can be simply omitted.

   "80" -> the same as "8" but with one important difference that the program if
     succeeded in creation of the "end.wav" file, then removes all other "result\*"
       files from the current directory, so you cannot reverse the successful action
         back and try again with just the same files. In such a case, you need to
           repeat all the process of creation of "result\*" files. Be aware and use
             with care!

  "82" -> the same as "8", but you can specify the sound quality parameters for the
     resulted file and choose whether it will be in WAV or FLAC format. To specify
       this additional processing information, use as a third command line argument
          (after the first "82" and the volatile second one) 4 consequent symbols:
            3 digits and 1 letter ('f' -- for FLAC one and 'w' for WAV one). The first
              two digits are a code for rate and the third one -- a code for bit depth
                ('1' -- for 16 bit and '2' -- for 24 bit). The list of possible first
                  two digits and their corresponding frequency rate in Hz:

                     "11" -> 11025

                     "16" -> 16000

                     "17" -> 176400

                     "19" -> 192000

                     "32" -> 32000

                     "44" -> 44100

                     "48" -> 48000

                     "80" -> 8000

                     "96" -> 96000

                     The default one behaviour is equivalent to "221w".

  "820" -> the same as "80", but similarly to "82" it gives you the opportunity to
      specify sound quality parameters in just the same way.

  "9", "99", or "999" -> see the information above.

  "92", "992", or "9992" -> similar to the above line, but with the opportunity to
      specify sound quality parameters in just the same way as for "82" option.
         These are considered still highly experimental and not well tested,
            so use them not for production.
           
   _  -> the program behaves like for the "5" option, but generates
     overtones using additional String and allows maximum control over
        the parameters.

After the program executing (it takes some time) with the first command line
options except "80" there are files "result\*.wav" in the directory.
These are the resulting melody generated in their order preserved.

The program now lifts the frequencies to the octave or to the enka with the number,
which you can specify during its execution.

You can use the default values (backward compatible with the 0.2.0.0 version 
if the first command line option does not consist of two digits and the last one
is not a '0' or '1') by simply pressing 'Enter' while being prompted and
the informational message contains the line about the default value.

Since the 0.21.0.0 version the package extends its library functions with
the possibility to create not only single notes or intervals of sounds playing
simultaneously but also sets of three, four, five, six, seven or more sounds
played simultaneously with their overtones. For more information, please, refer
to the documentation for the DobutokO.Sound.Functional module.

Since the 0.22.1.0 version the library functions are extended also with the
possibility to adjust volume for the overtones using generalized functions '1G'
with adjustment being represented in dB. For more information, please, refer
to the documentation for the DobutokO.Sound.Functional module.

Since the 0.23.0.0 version the library includes functions '2G': generalized
ones in the DobutokO.Sound.Functional, and DobutokO.Sound.IntermediateF, and
DobutokO.Sound.Executable modules. They allow to specify sound quality
of the resulting files using additional parameter.

Since the 0.24.0.0 version the library includes functions to work more explicitly
with f function in the DobutokO.Sound.Functional module. They are based on the
simplest (but still meaningful) multiplicative data fitting.

Since the 0.31.0.0 version the library includes functions to split the sound 
into several simultaneously sounding similar ones. For more information, please, 
refer to the DobutokO.Sound.Functional module.

Since the 0.36.0.0 version the library includes functions to provide your own 
variants of durations, intervals and volume adjustments and the possibilities 
to generate speech-like music with most of parameters obtained from the Ukrainian 
language texts.

Since the 0.38.0.0 version the library includes new datatype Params in the 
DobutokO.Sound.Functional module and various functions to work with it. It 
allows to use tonalities or something close to them. For more information, 
please, refer to DobutokO.Sound.Functional and DobutokO.Sound modules.

Since the 0.39.0.0 version there are some additional effects and opportunities 
in the DobutokO.Sound.Functional and DobutokO.Sound.IntermediateF modules. Some 
functions in the library were moved from the DobutokO.Sound.Executable module 
to the DobutokO.Sound.Functional module because of logics of the module structure 
and semantics.

** Note:

* Better to execute in the RAM. Need rather a lot of space on the disk for
the resulting file "end.wav" and auxiliary files (MBs) for a short sound
in the second command line arguments.

                ***** Examples *****
                ====================

Starting from the 0.24.0.0 version, you can refer to examples in the GitHub special
repository (https://github.com/OleksandrZhabenko/dobutokO2-examples/). 
You can also see some of the generated sounds on the YouTube list:
https://www.youtube.com/playlist?list=PLuG3zSZWV7yollV9nMPcRtm3udVuYVlFS


                ***** Support for not Sound File Variativity Sources *****
                ==========================================================
                
Since 0.28.0.0 version the library supports not only the sound file as a 
variativity source but also other types of files. They are used through 
a special reencoding using lazy bytestrings. For more information, please, 
refer to DobutokO.Sound.Keyboard module.                

Since 0.41.0.0 version the module structure is completely changed: there were added 
new ones and rearranged the existing, module DobutokO.Sound splitted into three 
different modules: DobutokO.Sound.Overtones, DobutokO.Sound.Uniq and 
DobutokO.Sound.Octaves. This was done to simplify compilation and to produce more 
logical and semantic structure. The most complicated functions from 
DobutokO.Sound.Functional module are moved to a new package dobutokO3. Some code 
improvements.

Since 0.42.0.0 version it uses Float instead of Double where possible. Double precision 
is redundant. The shift is inspired by: https://www.youtube.com/watch?v=FYTZkE5BZ-0

