            ***** Usage Notes *****
            =======================

For the proper proceeding you specify a String, which consists of 4 
digits (and it may be preceded by a symbol "-"). If the String 
begins with the "-", then a sound amplitude decreases, otherwise 
the amplitude increases. The level of increase / decrease is 
determined by the magnitude of the absolute value of integer numbers. 
The greater is the number -- the greater is the amplitude change. 
The count begins with "0000" and ends with "9999" (the sign is not 
taken into consideration). If there is less than 4 digits in a String, 
then the String is equivalent to that one with the appropriate number 
of zeroes preceding to fulfill to the 4 needed digits (for example, 
"657" is equivalent to "0657", "-2" is equivalent to "-0002" etc.). 
In such a case, for the Strings without the initial sign "-" (the 
sound increases) an interval between the maximum by modulus value 
of the amlitude (which is represented by the parts of 1) and 1.0 
is divided into 10 equal parts (starting a count from 0) and then 
that one of them is selected, which has a number determined by the 
first digit in the String writing. Then (if specified further) 
the interval between this amplitude value and a value, which 
corresponds to the selection on the previous step the next 
first digit in the writing (for example, after "4" -- "5", 
after "7" --"8" etc.), greater by 1 than the actually selected one, 
is also divided again into 10 equal parts and that one is selected, 
which corresponds to the number determined by the second digit in the 
String writing (again beginning with "0" and ending with "9") and so on 
until the 4th level. The greater exactness is not needed because our 
hearing ability hardly distinguish such a subtle sound changes. If 
the String has as a first element the "-" Char (the sound decreases), 
then everything is analogously the same, but an interval between the 
maximum by modulus amplitude value and 0.0 is divided into 10 equal parts 
and so on.

         ***** Command Line Arguments *****
         ==================================
         
If you specify a command line argument (other than "-h", "-r", or "-v"),
it must be a sorted list of the Ukrainian sounds representations,
which can be obtained by using the mmsyn7s package. For more information,
please, refer to:
[mmsyn7s](https://hackage.haskell.org/package/mmsyn7s).

In such a case, the program will modify only the amplitudes for
those Ukrainian sounds representations, which are in the list.
Otherwise, the program will modify the amplitudes for all non-silent Ukrainian
sounds representations in the current directory.

The "-h" command line argument is used for the help informational message.

The "-v" command line argument is used for the version number.

The "-r" command line argument is used to work with "result*.wav" files to adjust
their volume. You can additionally specify the numbers for the first and for
the last files to be adjusted.

         ***** Library Functions to Edit Sound *****
         ===========================================

Since the 0.5.0.0 version from DobutokO.Sound.IntermediateF module (dobutokO2 package) 
some functions were moved here. They are in MMSyn7l module. This was done to allow 
their usage without the necessity to install dobutokO2 package (just using mmsyn7* 
series). Added new functions that use the moved ones.

