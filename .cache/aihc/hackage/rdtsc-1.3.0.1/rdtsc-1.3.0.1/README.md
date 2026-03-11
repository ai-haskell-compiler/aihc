# rdtsc - Binding for the rdtsc machine instruction

This small Cabal package provides a Haskell binding to the "rdtsc"
machine instruction on modern IA-32 processors.  This instruction can
be used to read the number of cycles since processor startup and gives
very accurate timing information.

----

Note: this package contains the file cycle.h by Matteo Frigo, which
provides access to cycle counters on several architectures.  The
license for this file is included in the file LICENSE in this package.

----

## Usage

Use

    cabal install rdtsc

for building and installing system-wide, or

    tar xzvf rdtsc-X.Y.Z.W.tar.gz
    cd rdtsc-X.Y.Z.W
    cabal sandbox init
    cabal install

for building and installing in a sandbox.

Just import module "System.CPUTime.Rdtsc" into your Haskell file and use

    ghc --make FILENAME

to compile your program.

The "tests" directory contains a small test program for the module and
demonstrates its usage.  There is also a C version for this test
program, to test the overheads of Haskell's FFI.

Happy Haskell hacking,
  Martin
