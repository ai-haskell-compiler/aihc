#ifndef AIHC_WASM_INTERNAL_H
#define AIHC_WASM_INTERNAL_H

#include "aihc_runtime.h"

/* The machine of the program. The Lir entry unit stores it when the program
   starts, and the P3 driver reads it to resume IO and to report the exit
   status. */
extern AihcMachine *aihc_machine;

/* The Lir entry unit exports these. Each returns 1 when the program has
   halted and 0 when every thread waits for IO. */
int32_t aihc_lir_program_start(void);
int32_t aihc_lir_program_resume(const AihcResume *resume);

#endif
