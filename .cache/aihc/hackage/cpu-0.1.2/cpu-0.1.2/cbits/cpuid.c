#include <stdint.h>

void cpuid(uint32_t eax, uint32_t ecx, uint32_t regs[4])
{
	asm volatile (
#ifdef __i386__
		"push %%ebx\n\t"
		"push %%edx\n\t"
#else
		"push %%rbx\n\t"
		"push %%rdx\n\t"
#endif
		"cpuid\n\t"
		"mov %%eax,(%4)\n\t"
		"mov %%ebx,4(%4)\n\t"
		"mov %%ecx,8(%4)\n\t"
		"mov %%edx,12(%4)\n\t"
#ifdef __i386__
		"pop %%edx\n\t"
		"pop %%ebx\n\t"
#else
		"pop %%rdx\n\t"
		"pop %%rbx\n\t"
#endif
		: "=a" (eax), "=c" (ecx)
		: "0" (eax), "1" (ecx), "S" (regs)
		: "memory"
	);
}
