#include "SFMT.h"

uint64_t gen_rand64_mix_wrap(void)
{
    return gen_rand64_mix();
}

double genrand_real2_wrap(void)
{
    return genrand_real2();
}

double genrand_res53_wrap()
{
    return genrand_res53();
}
