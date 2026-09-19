#include <stdint.h>

extern int64_t lir_answer(void);

int main(void) { return lir_answer() == 42 ? 0 : 1; }
