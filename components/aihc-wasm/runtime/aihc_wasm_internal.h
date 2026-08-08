#ifndef AIHC_WASM_INTERNAL_H
#define AIHC_WASM_INTERNAL_H

#include "aihc_runtime.h"

extern AihcMachine *aihc_machine;

int aihc_wasm_pump_transfers(void);
void aihc_wasm_complete_io(int64_t result);
void aihc_wasm_set_exit_status(int64_t status);
int aihc_wasm_exit_succeeded(void);

#endif
