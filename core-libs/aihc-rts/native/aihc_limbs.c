/* The limb kernels of Integer arithmetic.

   Each kernel reads n limbs at up, multiplies them by the limb v, and
   writes n limbs at rp. A limb is a 64-bit word, and the least significant
   limb is first. The kernels have the semantics of the GMP functions
   mpn_mul_1, mpn_addmul_1 and mpn_submul_1:

   - aihc_limbs_mul_1 sets rp to up * v.
   - aihc_limbs_addmul_1 adds up * v to rp.
   - aihc_limbs_submul_1 subtracts up * v from rp.

   Each kernel returns the limb that does not fit in n limbs: the carry,
   or for the subtraction the amount to subtract from the next limb. The
   count n can be zero. The ranges of mul_1 must not overlap. For the
   other two kernels, rp and up must be equal or must not overlap.

   The x86-64 and AArch64 kernels are hand-scheduled assembly. They do four
   limbs in each iteration and keep the carry in the flags in a group of
   four limbs. The x86-64 kernels use mulx (BMI2): the code that aihc
   generates for x86-64 also uses mulx. All other targets use the portable
   C kernels. */

#include <stdint.h>

uint64_t aihc_limbs_mul_1(uint64_t *rp, const uint64_t *up, int64_t n,
                          uint64_t v);
uint64_t aihc_limbs_addmul_1(uint64_t *rp, const uint64_t *up, int64_t n,
                             uint64_t v);
uint64_t aihc_limbs_submul_1(uint64_t *rp, const uint64_t *up, int64_t n,
                             uint64_t v);

#if defined(__APPLE__)
#define AIHC_LIMBS_SYMBOL(name) "_" #name
#else
#define AIHC_LIMBS_SYMBOL(name) #name
#endif

#define AIHC_LIMBS_ENTRY(name)                                                 \
  ".globl " AIHC_LIMBS_SYMBOL(name) "\n"                                       \
                                    ".p2align 4\n" AIHC_LIMBS_SYMBOL(          \
                                        name) ":\n"

#if defined(__x86_64__)

/* System V and Darwin: rdi = rp, rsi = up, rdx = n, rcx = v. mulx takes
   its multiplier in rdx. rax holds the carry limb. The remainder loop does
   n mod 4 limbs first. In the main loop, the first carry chain adds each
   high half to the next low half, and the second chain adds or subtracts
   the four sums at rp. mulx, lea and dec do not change the carry flag. */
__asm__(".text\n" AIHC_LIMBS_ENTRY(
    aihc_limbs_addmul_1) "  pushq %rbx\n"
                         "  pushq %rbp\n"
                         "  movq %rdx, %r8\n"
                         "  movq %rcx, %rdx\n"
                         "  xorl %eax, %eax\n"
                         "  movq %r8, %rcx\n"
                         "  andq $3, %rcx\n"
                         "  jz 2f\n"
                         "1:\n"
                         "  mulxq (%rsi), %r9, %r10\n"
                         "  addq %rax, %r9\n"
                         "  adcq $0, %r10\n"
                         "  addq %r9, (%rdi)\n"
                         "  adcq $0, %r10\n"
                         "  movq %r10, %rax\n"
                         "  leaq 8(%rsi), %rsi\n"
                         "  leaq 8(%rdi), %rdi\n"
                         "  decq %rcx\n"
                         "  jnz 1b\n"
                         "2:\n"
                         "  shrq $2, %r8\n"
                         "  jz 4f\n"
                         "3:\n"
                         "  mulxq (%rsi), %r9, %r10\n"
                         "  addq %rax, %r9\n"
                         "  mulxq 8(%rsi), %r11, %rbx\n"
                         "  adcq %r10, %r11\n"
                         "  mulxq 16(%rsi), %rcx, %r10\n"
                         "  adcq %rbx, %rcx\n"
                         "  mulxq 24(%rsi), %rbp, %rax\n"
                         "  adcq %r10, %rbp\n"
                         "  adcq $0, %rax\n"
                         "  addq %r9, (%rdi)\n"
                         "  adcq %r11, 8(%rdi)\n"
                         "  adcq %rcx, 16(%rdi)\n"
                         "  adcq %rbp, 24(%rdi)\n"
                         "  adcq $0, %rax\n"
                         "  leaq 32(%rsi), %rsi\n"
                         "  leaq 32(%rdi), %rdi\n"
                         "  decq %r8\n"
                         "  jnz 3b\n"
                         "4:\n"
                         "  popq %rbp\n"
                         "  popq %rbx\n"
                         "  retq\n");

__asm__(".text\n" AIHC_LIMBS_ENTRY(
    aihc_limbs_submul_1) "  pushq %rbx\n"
                         "  pushq %rbp\n"
                         "  movq %rdx, %r8\n"
                         "  movq %rcx, %rdx\n"
                         "  xorl %eax, %eax\n"
                         "  movq %r8, %rcx\n"
                         "  andq $3, %rcx\n"
                         "  jz 2f\n"
                         "1:\n"
                         "  mulxq (%rsi), %r9, %r10\n"
                         "  addq %rax, %r9\n"
                         "  adcq $0, %r10\n"
                         "  subq %r9, (%rdi)\n"
                         "  adcq $0, %r10\n"
                         "  movq %r10, %rax\n"
                         "  leaq 8(%rsi), %rsi\n"
                         "  leaq 8(%rdi), %rdi\n"
                         "  decq %rcx\n"
                         "  jnz 1b\n"
                         "2:\n"
                         "  shrq $2, %r8\n"
                         "  jz 4f\n"
                         "3:\n"
                         "  mulxq (%rsi), %r9, %r10\n"
                         "  addq %rax, %r9\n"
                         "  mulxq 8(%rsi), %r11, %rbx\n"
                         "  adcq %r10, %r11\n"
                         "  mulxq 16(%rsi), %rcx, %r10\n"
                         "  adcq %rbx, %rcx\n"
                         "  mulxq 24(%rsi), %rbp, %rax\n"
                         "  adcq %r10, %rbp\n"
                         "  adcq $0, %rax\n"
                         "  subq %r9, (%rdi)\n"
                         "  sbbq %r11, 8(%rdi)\n"
                         "  sbbq %rcx, 16(%rdi)\n"
                         "  sbbq %rbp, 24(%rdi)\n"
                         "  adcq $0, %rax\n"
                         "  leaq 32(%rsi), %rsi\n"
                         "  leaq 32(%rdi), %rdi\n"
                         "  decq %r8\n"
                         "  jnz 3b\n"
                         "4:\n"
                         "  popq %rbp\n"
                         "  popq %rbx\n"
                         "  retq\n");

__asm__(".text\n" AIHC_LIMBS_ENTRY(
    aihc_limbs_mul_1) "  pushq %rbx\n"
                      "  pushq %rbp\n"
                      "  movq %rdx, %r8\n"
                      "  movq %rcx, %rdx\n"
                      "  xorl %eax, %eax\n"
                      "  movq %r8, %rcx\n"
                      "  andq $3, %rcx\n"
                      "  jz 2f\n"
                      "1:\n"
                      "  mulxq (%rsi), %r9, %r10\n"
                      "  addq %rax, %r9\n"
                      "  adcq $0, %r10\n"
                      "  movq %r9, (%rdi)\n"
                      "  movq %r10, %rax\n"
                      "  leaq 8(%rsi), %rsi\n"
                      "  leaq 8(%rdi), %rdi\n"
                      "  decq %rcx\n"
                      "  jnz 1b\n"
                      "2:\n"
                      "  shrq $2, %r8\n"
                      "  jz 4f\n"
                      "3:\n"
                      "  mulxq (%rsi), %r9, %r10\n"
                      "  addq %rax, %r9\n"
                      "  mulxq 8(%rsi), %r11, %rbx\n"
                      "  adcq %r10, %r11\n"
                      "  mulxq 16(%rsi), %rcx, %r10\n"
                      "  adcq %rbx, %rcx\n"
                      "  mulxq 24(%rsi), %rbp, %rax\n"
                      "  adcq %r10, %rbp\n"
                      "  adcq $0, %rax\n"
                      "  movq %r9, (%rdi)\n"
                      "  movq %r11, 8(%rdi)\n"
                      "  movq %rcx, 16(%rdi)\n"
                      "  movq %rbp, 24(%rdi)\n"
                      "  leaq 32(%rsi), %rsi\n"
                      "  leaq 32(%rdi), %rdi\n"
                      "  decq %r8\n"
                      "  jnz 3b\n"
                      "4:\n"
                      "  popq %rbp\n"
                      "  popq %rbx\n"
                      "  retq\n");

#elif defined(__aarch64__)

/* AAPCS64 and Darwin: x0 = rp, x1 = up, x2 = n, x3 = v. x4 holds the
   carry limb. The kernels use only the registers x0 to x17, which a call
   does not keep, thus they save no register. In AArch64, the carry flag
   is set when a subtraction does not borrow, thus the subtraction kernel
   adds one to the carry limb when the flag is clear. */
__asm__(
    ".text\n" AIHC_LIMBS_ENTRY(aihc_limbs_addmul_1) "  mov x4, #0\n"
                                                    "  and x5, x2, #3\n"
                                                    "  cbz x5, 2f\n"
                                                    "1:\n"
                                                    "  ldr x6, [x1], #8\n"
                                                    "  mul x7, x6, x3\n"
                                                    "  umulh x8, x6, x3\n"
                                                    "  adds x7, x7, x4\n"
                                                    "  adc x8, x8, xzr\n"
                                                    "  ldr x9, [x0]\n"
                                                    "  adds x9, x9, x7\n"
                                                    "  adc x4, x8, xzr\n"
                                                    "  str x9, [x0], #8\n"
                                                    "  sub x5, x5, #1\n"
                                                    "  cbnz x5, 1b\n"
                                                    "2:\n"
                                                    "  lsr x2, x2, #2\n"
                                                    "  cbz x2, 4f\n"
                                                    "3:\n"
                                                    "  ldp x6, x7, [x1], #16\n"
                                                    "  ldp x8, x9, [x1], #16\n"
                                                    "  mul x10, x6, x3\n"
                                                    "  umulh x11, x6, x3\n"
                                                    "  mul x12, x7, x3\n"
                                                    "  umulh x13, x7, x3\n"
                                                    "  mul x14, x8, x3\n"
                                                    "  umulh x15, x8, x3\n"
                                                    "  mul x16, x9, x3\n"
                                                    "  umulh x17, x9, x3\n"
                                                    "  adds x10, x10, x4\n"
                                                    "  adcs x12, x12, x11\n"
                                                    "  adcs x14, x14, x13\n"
                                                    "  adcs x16, x16, x15\n"
                                                    "  adc x4, x17, xzr\n"
                                                    "  ldp x6, x7, [x0]\n"
                                                    "  ldp x8, x9, [x0, #16]\n"
                                                    "  adds x6, x6, x10\n"
                                                    "  adcs x7, x7, x12\n"
                                                    "  adcs x8, x8, x14\n"
                                                    "  adcs x9, x9, x16\n"
                                                    "  adc x4, x4, xzr\n"
                                                    "  stp x8, x9, [x0, #16]\n"
                                                    "  stp x6, x7, [x0], #32\n"
                                                    "  sub x2, x2, #1\n"
                                                    "  cbnz x2, 3b\n"
                                                    "4:\n"
                                                    "  mov x0, x4\n"
                                                    "  ret\n");

__asm__(
    ".text\n" AIHC_LIMBS_ENTRY(aihc_limbs_submul_1) "  mov x4, #0\n"
                                                    "  and x5, x2, #3\n"
                                                    "  cbz x5, 2f\n"
                                                    "1:\n"
                                                    "  ldr x6, [x1], #8\n"
                                                    "  mul x7, x6, x3\n"
                                                    "  umulh x8, x6, x3\n"
                                                    "  adds x7, x7, x4\n"
                                                    "  adc x8, x8, xzr\n"
                                                    "  ldr x9, [x0]\n"
                                                    "  subs x9, x9, x7\n"
                                                    "  cinc x4, x8, cc\n"
                                                    "  str x9, [x0], #8\n"
                                                    "  sub x5, x5, #1\n"
                                                    "  cbnz x5, 1b\n"
                                                    "2:\n"
                                                    "  lsr x2, x2, #2\n"
                                                    "  cbz x2, 4f\n"
                                                    "3:\n"
                                                    "  ldp x6, x7, [x1], #16\n"
                                                    "  ldp x8, x9, [x1], #16\n"
                                                    "  mul x10, x6, x3\n"
                                                    "  umulh x11, x6, x3\n"
                                                    "  mul x12, x7, x3\n"
                                                    "  umulh x13, x7, x3\n"
                                                    "  mul x14, x8, x3\n"
                                                    "  umulh x15, x8, x3\n"
                                                    "  mul x16, x9, x3\n"
                                                    "  umulh x17, x9, x3\n"
                                                    "  adds x10, x10, x4\n"
                                                    "  adcs x12, x12, x11\n"
                                                    "  adcs x14, x14, x13\n"
                                                    "  adcs x16, x16, x15\n"
                                                    "  adc x4, x17, xzr\n"
                                                    "  ldp x6, x7, [x0]\n"
                                                    "  ldp x8, x9, [x0, #16]\n"
                                                    "  subs x6, x6, x10\n"
                                                    "  sbcs x7, x7, x12\n"
                                                    "  sbcs x8, x8, x14\n"
                                                    "  sbcs x9, x9, x16\n"
                                                    "  cinc x4, x4, cc\n"
                                                    "  stp x8, x9, [x0, #16]\n"
                                                    "  stp x6, x7, [x0], #32\n"
                                                    "  sub x2, x2, #1\n"
                                                    "  cbnz x2, 3b\n"
                                                    "4:\n"
                                                    "  mov x0, x4\n"
                                                    "  ret\n");

__asm__(
    ".text\n" AIHC_LIMBS_ENTRY(aihc_limbs_mul_1) "  mov x4, #0\n"
                                                 "  and x5, x2, #3\n"
                                                 "  cbz x5, 2f\n"
                                                 "1:\n"
                                                 "  ldr x6, [x1], #8\n"
                                                 "  mul x7, x6, x3\n"
                                                 "  umulh x8, x6, x3\n"
                                                 "  adds x7, x7, x4\n"
                                                 "  adc x4, x8, xzr\n"
                                                 "  str x7, [x0], #8\n"
                                                 "  sub x5, x5, #1\n"
                                                 "  cbnz x5, 1b\n"
                                                 "2:\n"
                                                 "  lsr x2, x2, #2\n"
                                                 "  cbz x2, 4f\n"
                                                 "3:\n"
                                                 "  ldp x6, x7, [x1], #16\n"
                                                 "  ldp x8, x9, [x1], #16\n"
                                                 "  mul x10, x6, x3\n"
                                                 "  umulh x11, x6, x3\n"
                                                 "  mul x12, x7, x3\n"
                                                 "  umulh x13, x7, x3\n"
                                                 "  mul x14, x8, x3\n"
                                                 "  umulh x15, x8, x3\n"
                                                 "  mul x16, x9, x3\n"
                                                 "  umulh x17, x9, x3\n"
                                                 "  adds x10, x10, x4\n"
                                                 "  adcs x12, x12, x11\n"
                                                 "  adcs x14, x14, x13\n"
                                                 "  adcs x16, x16, x15\n"
                                                 "  adc x4, x17, xzr\n"
                                                 "  stp x14, x16, [x0, #16]\n"
                                                 "  stp x10, x12, [x0], #32\n"
                                                 "  sub x2, x2, #1\n"
                                                 "  cbnz x2, 3b\n"
                                                 "4:\n"
                                                 "  mov x0, x4\n"
                                                 "  ret\n");

#else

/* The product of two limbs as a high and a low limb. */
static inline void aihc_limbs_product(uint64_t left, uint64_t right,
                                      uint64_t *high, uint64_t *low) {
#if defined(__SIZEOF_INT128__)
  __extension__ typedef unsigned __int128 aihc_limbs_wide;
  aihc_limbs_wide product = (aihc_limbs_wide)left * right;
  *high = (uint64_t)(product >> 64);
  *low = (uint64_t)product;
#else
  uint64_t leftLow = left & 0xffffffffu;
  uint64_t leftHigh = left >> 32;
  uint64_t rightLow = right & 0xffffffffu;
  uint64_t rightHigh = right >> 32;
  uint64_t lowLow = leftLow * rightLow;
  uint64_t highLow = leftHigh * rightLow;
  uint64_t lowHigh = leftLow * rightHigh;
  uint64_t highHigh = leftHigh * rightHigh;
  uint64_t middle = (lowLow >> 32) + (highLow & 0xffffffffu) + lowHigh;
  *high = highHigh + (highLow >> 32) + (middle >> 32);
  *low = (middle << 32) | (lowLow & 0xffffffffu);
#endif
}

uint64_t aihc_limbs_mul_1(uint64_t *rp, const uint64_t *up, int64_t n,
                          uint64_t v) {
  uint64_t carry = 0;
  for (int64_t index = 0; index < n; index++) {
    uint64_t high;
    uint64_t low;
    aihc_limbs_product(up[index], v, &high, &low);
    low += carry;
    rp[index] = low;
    carry = high + (low < carry);
  }
  return carry;
}

uint64_t aihc_limbs_addmul_1(uint64_t *rp, const uint64_t *up, int64_t n,
                             uint64_t v) {
  uint64_t carry = 0;
  for (int64_t index = 0; index < n; index++) {
    uint64_t high;
    uint64_t low;
    aihc_limbs_product(up[index], v, &high, &low);
    low += carry;
    high += low < carry;
    uint64_t result = rp[index] + low;
    rp[index] = result;
    carry = high + (result < low);
  }
  return carry;
}

uint64_t aihc_limbs_submul_1(uint64_t *rp, const uint64_t *up, int64_t n,
                             uint64_t v) {
  uint64_t carry = 0;
  for (int64_t index = 0; index < n; index++) {
    uint64_t high;
    uint64_t low;
    aihc_limbs_product(up[index], v, &high, &low);
    low += carry;
    high += low < carry;
    uint64_t word = rp[index];
    rp[index] = word - low;
    carry = high + (word < low);
  }
  return carry;
}

#endif
