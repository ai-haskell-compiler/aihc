#include <stdio.h>
#include <inttypes.h>

/* http://www.haskell.org/haskellwiki/FFICookBook#Working_with_structs */
#define hsc_alignment(t) \
    printf("(%ld)", (unsigned long)offsetof(struct {char x__; t (y__); }, y__));

#define hsc_inttype(t) \
    printf ("%s%lu",                           \
            (t)(-1) < (t)0 ? "Int" : "Word",   \
            (unsigned long)sizeof (t) * 8);

#define hsc_intfieldtype(t,f) \
    { \
        t x, y;    \
        x.f = -1;  \
        y.f = 0;   \
        printf ("%s%lu",                           \
                x.f < y.f ? "Int" : "Word",        \
                (unsigned long)sizeof (x.f) * 8);  \
    }

#define hsc_peek_int(t, f) \
    { \
        t *x = 0; \
        printf ("(\\hsc_ptr -> fmap fromIntegral (peekByteOff hsc_ptr %ld :: IO ", \
            (uintptr_t) &(x->f)); \
        hsc_intfieldtype (t, f) \
        printf ("))"); \
    }

#define hsc_poke_int(t, f) \
    { \
        t *x = 0; \
        printf ("(\\hsc_ptr hsc_x -> pokeByteOff hsc_ptr %ld (fromIntegral hsc_x :: ", \
            (uintptr_t) &(x->f)); \
        hsc_intfieldtype (t, f) \
        printf ("))"); \
    }
