#include <stdint.h>

uint64_t check_integers(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                        uint64_t a4, uint64_t a5, uint64_t a6, uint64_t a7,
                        uint8_t a8, uint16_t a9, uint32_t a10, void *a11,
                        uint64_t a12) {
  return a0 == 1ULL && a1 == 2ULL && a2 == 3ULL && a3 == 4ULL && a4 == 5ULL &&
         a5 == 6ULL && a6 == 7ULL && a7 == 8ULL && a8 == 255ULL &&
         a9 == 65530ULL && a10 == 4294967290ULL && a12 == 13ULL &&
         *(uint64_t *)a11 == 99;
}
