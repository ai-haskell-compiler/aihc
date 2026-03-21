#define OUTER 1
#define INNER 1
#if OUTER
outer-true
#if INNER
inner-true
#else
inner-false
#endif
#else
outer-false
#endif
