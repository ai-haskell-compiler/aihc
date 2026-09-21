#include <stdint.h>

extern uint64_t forward_tail(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                             uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                             uint64_t a8, uint64_t a9, uint64_t a10,
                             uint64_t a11, uint64_t a12);
extern uint64_t forward_indirect(uint64_t a0, uint64_t a1, uint64_t a2,
                                 uint64_t a3, uint64_t a4, uint64_t a5,
                                 uint64_t a6, uint64_t a7, uint64_t a8,
                                 uint64_t a9, uint64_t a10, uint64_t a11,
                                 uint64_t a12);

uint64_t check_reverse(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                       uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                       uint64_t a8, uint64_t a9, uint64_t a10, uint64_t a11,
                       uint64_t a12) {
  return a0 == 13 && a1 == 12 && a2 == 11 && a3 == 10 && a4 == 9 && a5 == 8 &&
         a6 == 7 && a7 == 6 && a8 == 5 && a9 == 4 && a10 == 3 && a11 == 2 &&
         a12 == 1;
}

uint64_t check_reuse(void) {
  return forward_tail(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13) &&
         forward_indirect(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13);
}
