/* A constant of the package's own, so that hsc2hs needs no system header:
   the test runs cross-compiling to Darwin, where a host glibc header is
   not the compiler's to read. */
#define BROKEN_ANSWER 42
