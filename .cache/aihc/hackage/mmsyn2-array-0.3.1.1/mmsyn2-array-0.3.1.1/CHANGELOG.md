# Revision history for mmsyn2-array

## 0.1.0.0 -- 2020-12-14

* First version. Released on an unsuspecting world.

## 0.1.0.1 -- 2020-12-14

* First version revised A. Some documentation improvements.

## 0.1.1.0 -- 2020-12-18

* First version revised B. Fixed issue with the first and last elements of the array that were not taken into account.

## 0.1.2.0 -- 2021-03-29

* First version revised C. Some code optimization improvements. Changed the inner implementation and imported modules.

## 0.1.3.0 -- 2021-03-29

* First version revised D. Fixed issues with being not compiled for GHC < 8.0* series.

## 0.2.0.0 -- 2021-10-30

* Second version. Added the module Case.Hashable.Cuckoo variant based on the Hashtable (cuckoo hash) usage.
Added the respective dependency.

## 0.2.1.0 -- 2021-10-30

* Second version revised A. Discontinued the base < 4.9 versions.

## 0.2.1.1 -- 2021-10-30

* Second version revised B. Added inlining of the Case.Hashable.Cuckoo.getBFstL' function.

## 0.2.2.0 -- 2021-10-30

* Second version revised C. Added new functions to work with lists and arrays to the Case.Hashable.Cuckoo module.

## 0.3.0.0 -- 2021-10-31

* Third version. The hashing functionality with its dependencies moved to a new 
package mmsyn2-hashable. 

## 0.3.1.0 -- 2023-01-28

* Third version revised A. Switched to NoImplicitPrelude extension usage. Some code improvements.
Updated the boundaries for dependencies.

## 0.3.1.1 -- 2023-01-29

* Third version revised B. Some documentation improvements.
