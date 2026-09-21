#include <stdint.h>

double check_mixed(uint64_t a0, double a1, uint64_t a2, double a3, uint64_t a4,
                   double a5, uint64_t a6, double a7, uint64_t a8, double a9,
                   uint64_t a10, double a11, uint64_t a12, double a13,
                   uint64_t a14, double a15, uint8_t a16, float a17,
                   uint16_t a18, double a19, uint32_t a20, float a21, void *a22,
                   uint64_t a23) {
  return (a0 == 1 && a1 == 1.25 && a2 == 2 && a3 == 2.25 && a4 == 3 &&
          a5 == 3.25 && a6 == 4 && a7 == 4.25 && a8 == 5 && a9 == 5.25 &&
          a10 == 6 && a11 == 6.25 && a12 == 7 && a13 == 7.25 && a14 == 8 &&
          a15 == 8.25 && a16 == 255 && a17 == 9.5 && a18 == 65530 &&
          a19 == 10.25 && a20 == 4294967290 && a21 == 11.5 && a23 == 24 &&
          *(uint64_t *)a22 == 99)
             ? 42.5
             : 0.0;
}
