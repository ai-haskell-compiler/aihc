#ifndef AIHC_RUNTIME_TRAMPOLINE_H
#define AIHC_RUNTIME_TRAMPOLINE_H

#include "aihc_runtime.h"

/* Array-based control transfers are used by the WebAssembly fallback adapter.
 */
typedef struct {
  AihcEntry entry;
  AihcSlot *arguments;
} AihcTrampolineTransfer;

AihcTrampolineTransfer aihc_trampoline_call(AihcMachine *machine,
                                            AihcEntry entry, uint64_t count,
                                            const AihcSlot *arguments);
AihcTrampolineTransfer aihc_trampoline_apply_cps(AihcMachine *machine,
                                                 AihcValue *function,
                                                 uint64_t count,
                                                 const AihcSlot *arguments,
                                                 AihcValue *continuation);
AihcTrampolineTransfer aihc_trampoline_eval_cps(AihcMachine *machine,
                                                AihcValue *value,
                                                uint64_t result_is_lifted,
                                                AihcValue *continuation,
                                                AihcValue *update_continuation);
AihcTrampolineTransfer aihc_trampoline_continue_values(AihcMachine *machine,
                                                       AihcValue *continuation,
                                                       uint64_t count,
                                                       const AihcSlot *values);
AihcTrampolineTransfer aihc_trampoline_raise_cps(AihcMachine *machine,
                                                 AihcValue *exception,
                                                 AihcValue *continuation);
AihcTrampolineTransfer aihc_trampoline_fork_cps(AihcMachine *machine,
                                                AihcValue *action,
                                                AihcValue *continuation);
AihcTrampolineTransfer aihc_trampoline_new_mvar_cps(AihcMachine *machine,
                                                    AihcValue *continuation);
AihcTrampolineTransfer aihc_trampoline_read_mvar_cps(AihcMachine *machine,
                                                     void *mvar,
                                                     AihcValue *continuation);
AihcTrampolineTransfer aihc_trampoline_take_mvar_cps(AihcMachine *machine,
                                                     void *mvar,
                                                     AihcValue *continuation);
AihcTrampolineTransfer aihc_trampoline_put_mvar_cps(AihcMachine *machine,
                                                    void *mvar,
                                                    AihcValue *value,
                                                    AihcValue *continuation);
AihcTrampolineTransfer aihc_trampoline_yield_cps(AihcMachine *machine,
                                                 AihcValue *continuation);
AihcTrampolineTransfer aihc_trampoline_await_io_cps(AihcMachine *machine,
                                                    void *request,
                                                    AihcValue *continuation);
AihcTrampolineTransfer aihc_trampoline_thread_done(AihcMachine *machine);
AihcTrampolineTransfer
aihc_trampoline_start(AihcMachine *machine, AihcValue *root,
                      AihcValue *continuation, AihcValue *update_continuation,
                      AihcValue *thread_done_continuation, AihcEntry exit_code);
AihcTrampolineTransfer aihc_trampoline_resume(AihcMachine *machine,
                                              const AihcResume *resume);

#endif
