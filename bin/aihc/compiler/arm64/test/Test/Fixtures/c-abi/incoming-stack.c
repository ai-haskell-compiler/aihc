#include <stdint.h>

extern uint64_t receive_mixed(uint64_t a0, double a1, uint64_t a2, double a3,
                              uint64_t a4, double a5, uint64_t a6, double a7,
                              uint64_t a8, double a9, uint64_t a10, double a11,
                              uint64_t a12, double a13, uint64_t a14,
                              double a15, uint8_t a16, float a17, uint16_t a18,
                              double a19, uint32_t a20, float a21, void *a22,
                              uint64_t a23);

uint64_t check_incoming(void) {
  uint64_t cell = 99;
  return receive_mixed(1, 1.25, 2, 2.25, 3, 3.25, 4, 4.25, 5, 5.25, 6, 6.25, 7,
                       7.25, 8, 8.25, 255, 9.5, 65530, 10.25, 4294967290, 11.5,
                       &cell, 24);
}
