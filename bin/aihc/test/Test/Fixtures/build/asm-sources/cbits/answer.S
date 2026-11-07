// The function aihc_asm_answer returns 42. The C preprocessor selects the
// instructions of the target and the symbol name of its object format.
#if defined(__APPLE__)
#define SYMBOL(name) _##name
#else
#define SYMBOL(name) name
#endif

	.text
	.globl SYMBOL(aihc_asm_answer)
	.p2align 4
SYMBOL(aihc_asm_answer):
#if defined(__aarch64__)
	mov w0, #42
	ret
#elif defined(__x86_64__)
	movl $42, %eax
	ret
#else
#error "The fixture has no instructions for this target."
#endif

#if defined(__ELF__)
	.section .note.GNU-stack,"",%progbits
#endif
