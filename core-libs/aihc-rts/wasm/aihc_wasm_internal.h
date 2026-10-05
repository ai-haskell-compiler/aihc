#ifndef AIHC_WASM_INTERNAL_H
#define AIHC_WASM_INTERNAL_H

#include "aihc_runtime.h"

/* A handle token at least this large is an open HTTP response. No
   descriptor of a preopened directory reaches it. */
#define AIHC_HTTP_TOKEN_BASE ((int32_t)1 << 20)

/* The Lir entry unit exports these. Each returns 1 when the program has
   halted and 0 when every thread waits for IO. */
int32_t aihc_lir_program_start(void);
int32_t aihc_lir_program_resume(const AihcResume *resume);

#endif
