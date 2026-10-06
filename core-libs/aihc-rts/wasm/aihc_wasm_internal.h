#ifndef AIHC_WASM_INTERNAL_H
#define AIHC_WASM_INTERNAL_H

#include "aihc_runtime.h"

/* A handle token at least this large is an open HTTP response. No
   descriptor of a preopened directory reaches it. */
#define AIHC_HTTP_TOKEN_BASE ((int32_t)1 << 20)

/* A handle token at least this large is a descriptor of the libc, which the
   program got from a libc call such as open, and wrapped as a Handle. The
   token is the descriptor plus this number, so that it cannot be taken for a
   WASI resource or for an HTTP response. */
#define AIHC_LIBC_FD_TOKEN_BASE ((uintptr_t)1 << 24)

/* The Lir entry unit exports these. Each returns 1 when the program has
   halted and 0 when every thread waits for IO. */
int32_t aihc_lir_program_start(void);
int32_t aihc_lir_program_resume(const AihcResume *resume);

#endif
