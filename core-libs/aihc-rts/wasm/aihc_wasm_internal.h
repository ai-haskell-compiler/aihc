#ifndef AIHC_WASM_INTERNAL_H
#define AIHC_WASM_INTERNAL_H

#include "aihc_runtime.h"

/* The Lir entry unit exports these. Each returns 1 when the program has
   halted and 0 when every thread waits for IO. */
int32_t aihc_lir_program_start(void);
int32_t aihc_lir_program_resume(const AihcResume *resume);

#endif
