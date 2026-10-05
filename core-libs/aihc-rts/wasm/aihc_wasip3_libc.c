/* The functions that the WASI 0.3 libc expects its toolchain to supply.

   The libc finds its stack pointer and its thread-local storage through
   functions, so that a component with several tasks can keep one of each per
   task, in the context slots of the component model. The runtime runs the
   whole program in one task, with the stack pointer in the global that
   wasm-ld makes, and the linker lays the thread-local segment out once. The
   functions read those. */

// NOLINTBEGIN(bugprone-reserved-identifier)
void *__wasm_get_stack_pointer(void) {
  void *pointer;
  __asm__ volatile(".globaltype __stack_pointer, i32\n\t"
                   "global.get __stack_pointer\n\t"
                   "local.set %0"
                   : "=r"(pointer));
  return pointer;
}

void __wasm_set_stack_pointer(void *pointer) {
  __asm__ volatile(".globaltype __stack_pointer, i32\n\t"
                   "local.get %0\n\t"
                   "global.set __stack_pointer"
                   :
                   : "r"(pointer));
}

void *__wasm_get_tls_base(void) {
  void *base;
  __asm__ volatile(".globaltype __tls_base, i32\n\t"
                   "global.get __tls_base\n\t"
                   "local.set %0"
                   : "=r"(base));
  return base;
}
// NOLINTEND(bugprone-reserved-identifier)
