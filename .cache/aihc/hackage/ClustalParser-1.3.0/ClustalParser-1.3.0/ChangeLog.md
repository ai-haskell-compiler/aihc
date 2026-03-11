-*-change-log-*-

### 1.3.0 [Florian Eggenhofer](mailto:egg@cs.uni-freiburg.de) 14. November 2019

  * Fixed requested tick number for compilation with GHC 8.6.*
  * Changed to Biobase style

### 1.2.3 [Florian Eggenhofer](mailto:egg@cs.uni-freiburg.de) 12. March 2018

  * Fixed parsing of additional newline in Biopythons AlignIO output without conservation track

### 1.2.2 [Florian Eggenhofer](mailto:egg@cs.uni-freiburg.de) 07. March 2018

  * Clustal parser can now parse alignments with missing consensus annotation

### 1.2.1 [Florian Eggenhofer](mailto:egg@cs.uni-freiburg.de) 06. February 2017

  * Structural alignment parser now works with multiline consensus structures

### 1.2.0 [Florian Eggenhofer](mailto:egg@cs.uni-freiburg.de) 07. January 2017

  * Changed datastructures for sequence identifers and sequences to Data.Text

### 1.1.4 [Florian Eggenhofer](mailto:egg@cs.uni-freiburg.de) 30. May 2016

  * Fixed a bug in output of clustal alignments with sequence length of 60

### 1.1.3 [Florian Eggenhofer](mailto:florian.eggenhofer@univie.ac.at) 4. July 2015

  * Nucleotide sequences are now parsed by a unified function in line with IUPAC nucleotide code

### 1.1.2 [Florian Eggenhofer](mailto:florian.eggenhofer@univie.ac.at) 3. July 2015

  * Included parsing of optional field in mlocarna clustal output

### 1.1.1 [Florian Eggenhofer](mailto:florian.eggenhofer@univie.ac.at) 2. July 2015

  * Added support for cmalign clustal output .

### 1.1.0 [Florian Eggenhofer](mailto:florian.eggenhofer@univie.ac.at) 1. July 2015

  * Added Hspec test-suite for parsing functions
  * Added Show instances for ClustalAlignment and StructuralClustalAlignment

### 1.0.3 [Florian Eggenhofer](mailto:florian.eggenhofer@univie.ac.at) 19. April 2015

  * Added Y (pyrimidine) and R (purine) to sequence characters

### 1.0.2 [Florian Eggenhofer](mailto:florian.eggenhofer@univie.ac.at> 19. March 2015

	* Linebreaks are now filtered from structural alignment sequence identifiers

### 1.0.1 [Florian Eggenhofer](mailto:florian.eggenhofer@univie.ac.at> 27. October 2014

	* Fixed compiler warnings and updated documentation to mention structural clustal format
	* Added -Wall and -O2 compiler options
	* Added support for clustal alignments with secondary structure annotation
