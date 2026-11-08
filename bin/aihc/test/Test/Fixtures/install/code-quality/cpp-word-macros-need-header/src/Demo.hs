module Demo where

#if defined(WORD_SIZE_IN_BITS) || defined(SIZEOF_HSWORD)
#error the word macros need MachDeps.h
#endif

data Token = Token
