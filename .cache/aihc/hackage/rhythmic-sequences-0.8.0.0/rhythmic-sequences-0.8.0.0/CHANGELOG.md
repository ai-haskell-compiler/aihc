# Revision history for rhythmic-sequence

## 0.1.0.0 -- 2023-02-27

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2023-02-27

* First version revised A. Fixed issue with being not compiled by the GHC-9.2 and higher.

## 0.1.2.0 -- 2023-02-27

* First version revised B. Fixed another issue with being not compiled by the GHC-9.2 and higher.

## 0.1.2.1 -- 2023-02-27

* First version revised C. Some minor documentation improvements.

## 0.1.3.0 -- 2023-02-27

* First version revised D. Switched to sortOn instead of sortBy. The new variant can be more memory
consuming, but it is generally speaking not worse in speed and for complex computations can lead
to less computations.

## 0.2.0.0 -- 2023-03-10

* Second version. Added the functions to read the HashCorrections data and the other needed for
countHashesG function.

## 0.2.0.1 -- 2023-03-10

* Second version revised A. Some documentation improvement.

## 0.2.1.9 -- 2023-03-11

* Second version revised B. Changed the defaults for the new functions. Added the possibility to
  parse the '-' sign into the negative second parameter  in HashCorrections.

## 0.2.2.0 -- 2023-03-17

* Second version revised C. Some performance improvements for splitF function. Documentation 
improvements.

## 0.2.3.0 -- 2023-03-17

* Second version revised D. Fixed issue with undefined pattern matching. Modified the 
splitF code with the usage of the modified Data.List.unfoldr code to improve performance.
Some documentation improvements.

## 0.2.3.1 -- 2023-03-17

* Second version revised E. Some minor code improvement. Documentation improvements.

## 0.3.0.0 -- 2023-03-28

* Third version. Added two generalized functions to Rhythmicity.MarkersSeqs module and one 
- to Rhythmicity.BasicF module. They allow to specify step of second hashing. Changed also
the values in the hashList function for the positive values of the second parameter in 
the argument HashCorrections in it.

## 0.4.0.0 -- 2023-08-14

* Fourth version. Added a new function for working with points of incongruences showZerosFor2Period.

## 0.4.1.0 -- 2023-10-01

* Fourth version revised A. Added new function to be used in the "music" mode of PhLADiPreLiO.

## 0.8.0.0 -- 2024-04-07

* Eighth version. Switched to Word8 instead of Double. Made a versions window to provide possible updates for Double-related functionality. 

