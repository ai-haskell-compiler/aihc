#include <stdint.h>

uint64_t check_tail(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                    uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                    uint64_t a8, uint64_t a9, uint64_t a10, uint64_t a11,
                    uint64_t a12) {
  return a0 == 1 && a1 == 2 && a2 == 3 && a3 == 4 && a4 == 5 && a5 == 6 &&
         a6 == 7 && a7 == 8 && a8 == 9 && a9 == 10 && a10 == 11 && a11 == 12 &&
         a12 == 13;
}

double check_tail_float(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                        uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                        uint64_t a8, uint64_t a9, uint64_t a10, uint64_t a11,
                        uint64_t a12) {
  return (a0 == 1 && a1 == 2 && a2 == 3 && a3 == 4 && a4 == 5 && a5 == 6 &&
          a6 == 7 && a7 == 8 && a8 == 9 && a9 == 10 && a10 == 11 && a11 == 12 &&
          a12 == 13)
             ? 42.5
             : 0.0;
}

uint64_t check_small(uint64_t a) { return a == 13; }
