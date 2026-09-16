/* The constant is guarded on the macros Cabal writes into cabal_macros.h,
   which hsc2hs sees only if the install force-includes that header: an
   undefined function-like macro is a C error, not a false condition. */
#if __GLASGOW_HASKELL__ >= 900 && MIN_VERSION_ghc(9, 0, 0)
#define DEMO_ANSWER 42
#else
#define DEMO_ANSWER 0
#endif
