#include <stdio.h>

/* cf. FFI cookbook */
#if __GLASGOW_HASKELL__ < 800
#define hsc_alignment(t) \
    printf("(%lu)", (unsigned long)offsetof(struct {char x__; t (y__); }, y__));
#endif

#define hsc_inttype(t) \
    printf ("%s%lu",                                    \
            (t)(-1) < (t)0 ? "Int.Int" : "Word.Word",   \
            (unsigned long)sizeof (t) * 8);             \

#define hsc_intfieldtype(r,f) \
    { \
        r x, y;    \
        x.f = -1;  \
        y.f = 0;   \
        printf ("%s%lu",                               \
                x.f < y.f ? "Int.Int" : "Word.Word",   \
                (unsigned long)sizeof (x.f) * 8);      \
    }

#define hsc_newinttype(nm,t) \
    printf ("newtype "nm" = "nm" { un"nm" :: "); \
    hsc_inttype(t); \
    printf (" }\n"); \
    printf (" deriving (Show, Eq, Ord, Ix, Storable)\n");

#define hsc_newintfieldtype(nm,r,f) \
    printf ("newtype "nm" = "nm" { un"nm" :: "); \
    hsc_intfieldtype(r,f); \
    printf (" }\n"); \
    printf (" deriving (Show, Eq, Ord, Ix, Storable)\n");

#define hsc_peekintfield(t, f) \
    printf ("(\\hsc_ptr -> peekByteOff hsc_ptr %ld :: IO ", \
            (long) offsetof (t, f)); \
    hsc_intfieldtype(t,f); \
    printf (")");

#define hsc_pokeintfield(t, f) \
    printf ("(\\hsc_ptr -> pokeByteOff hsc_ptr %ld :: ", \
            (long) offsetof (t, f)); \
    hsc_intfieldtype(t,f); \
    printf (" -> IO ())");
