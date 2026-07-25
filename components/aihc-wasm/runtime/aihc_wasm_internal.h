#ifndef AIHC_WASM_INTERNAL_H
#define AIHC_WASM_INTERNAL_H

#include "aihc_runtime.h"

extern AihcMachine *aihc_machine;

int aihc_wasm_pump_transfers(void);
void aihc_wasm_complete_io(int32_t result);

#endif
