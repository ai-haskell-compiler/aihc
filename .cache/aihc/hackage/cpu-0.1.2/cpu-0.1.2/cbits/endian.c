#include <stdint.h>

#if (defined(__i386__))
#  define ARCH_HAS_SWAP32
uint32_t bitfn_swap32(uint32_t a)
{
	asm ("bswap %0" : "=r" (a) : "0" (a));
	return a;
}
/**********************************************************/
# elif (defined(__arm__))
#  define ARCH_HAS_SWAP32
uint32_t bitfn_swap32(uint32_t a)
{
	uint32_t tmp = a;
	asm volatile ("eor %1, %0, %0, ror #16\n"
	              "bic %1, %1, #0xff0000\n"
	              "mov %0, %0, ror #8\n"
	              "eor %0, %0, %1, lsr #8\n"
	              : "=r" (a), "=r" (tmp) : "0" (a), "1" (tmp));
	return a;
}
/**********************************************************/
# elif defined(__x86_64__)
#  define ARCH_HAS_SWAP32
#  define ARCH_HAS_SWAP64
uint32_t bitfn_swap32(uint32_t a)
{
	asm ("bswap %0" : "=r" (a) : "0" (a));
	return a;
}

uint64_t bitfn_swap64(uint64_t a)
{
	asm ("bswap %0" : "=r" (a) : "0" (a));
	return a;
}

# endif

#ifndef ARCH_HAS_SWAP32
uint32_t bitfn_swap32(uint32_t a)
{
	return (a << 24) | ((a & 0xff00) << 8) | ((a >> 8) & 0xff00) | (a >> 24);
}
#endif

#ifndef ARCH_HAS_SWAP64
uint64_t bitfn_swap64(uint64_t a)
{
	return ((uint64_t) bitfn_swap32((uint32_t) (a >> 32))) |
	        (((uint64_t) bitfn_swap32((uint32_t) a)) << 32);
}
#endif

uint16_t bitfn_swap16(uint16_t a)
{
  return (a << 8) | (a >> 8);
}
