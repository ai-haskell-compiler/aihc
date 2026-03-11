# Revision history for mmsyn7l

## 0.1.0.0 -- 2020-01-07

* First version. Released on an unsuspecting world.

## 0.1.0.1 -- 2020-01-07

* First version revised A. Some documentation minor improvements. 

## 0.1.1.0 -- 2020-01-08

* First version revised B. Fixed an issue with the wrong amplitude level recognition.

## 0.1.1.1 -- 2020-01-08

* First version revised C. Fixed an issue with the wrong written function call.

## 0.2.0.0 -- 2020-01-10

* Second version. Added a possibility to use a command line argument 
obtained e. g. from the usage of mmsyn7s package (for more information, 
please, refer to: https://hackage.haskell.org/package/mmsyn7s).

## 0.2.1.0 -- 2020-01-13

* Second version revised A. Fixed issues with wrongly parsed sounds that are represented
by two letters ("дж", "дз", "сь", "ць").

## 0.2.2.0 -- 2020-01-13

* Second version revised B. Simplified the main function using the simlified version of
the ReplaceP mudule in the mmsyn7ukr package.

## 0.2.3.0 -- 2020-01-13

* Second version revised C. Fixed an issue with the wrong dependency mmsyn7ukr>=0.6.2.1.

## 0.3.0.0 -- 2020-01-28

* Third version. Changed the dependency bounds. Added the support for informational messages. Some documentation improvements.

## 0.3.1.0 -- 2020-01-28

* Third version revised A. Changed errors to exceptions. Some documentation improvements.

## 0.3.2.0 -- 2020-01-30

* Third version revised B. Changed README file to README.markdown.

## 0.4.0.0 -- 2020-03-25

* Fourth version. Added new command line option variant "-r" and changed MMSyn7l.changeVolume function. Added for this purpose a new function 'adjustVolRes'.
Added directory as an explicit dependency (earlier it has been implicit as a dependency for mmsyn7ukr package). Changed information in the README.markdown
file appropriately.

## 0.4.1.0 -- 2020-03-25

* Fourth version revised A. Fixed issues with being not compiled for GHC 7.8.4 and for the wrong arguments specifying for 'adjustVolRes' function.

## 0.4.2.0 -- 2020-03-25

* Fourth version revised B. Fixed issue with wrongly specified interval to be adjusted with 'adjustVolRes' function.

## 0.4.3.0 -- 2020-05-14

* Fourth version revised C. Changed bounds for the dependencies so that now also GHC 8.10* series are supported.

## 0.5.0.0 -- 2020-05-20

* Fifth version. From DobutokO.Sound.IntermediateF module (dobutokO2 package) some functions were moved here. They are in MMSyn7l module. This was 
done to allow their usage without the necessity to install dobutokO2 package (just using mmsyn7* series). Added new fade functions to the same 
module using the moved ones.

## 0.6.0.0 -- 2020-05-30

* Sixth version. Added two new functions to fade ends of the sound to MMSyn7l module. Fixed an issue with the version number in the Main.main function. 
Some minor code and documentation improvements.

## 0.6.1.0 -- 2020-05-30

* Sixth version revised A. Fixed issue with being not compiled because of the syntaxis.

## 0.7.0.0 -- 2020-06-24

* Seventh version. Changed Double to Float in the MMSyn7l module so that the functons can be consistent with new dobutokO packages. Besides, the double precision 
is not needed and in some cases is meaningless and can potentially (a little bit, however) reduce performance in some cases. Some minor code and documentation improvements.

## 0.8.0.0 -- 2020-08-16

* Eighth version. Changed the dependency of mmsyn7ukr bounds.

## 0.9.0.0 -- 2021-02-15

* Ninth version. Removed vector-related dependencies. Some code improvements.

## 0.9.1.0 -- 2022-08-16

* Ninth version revised. Updated the dependencies to support the newer GHC-9* serios. Some minor 
code improvements.

## 0.9.2.0 -- 2024-12-19

* Ninth version revised B. Updated the dependencies. Added github repository and bug-tracker.

