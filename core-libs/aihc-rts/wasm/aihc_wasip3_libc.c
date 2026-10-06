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

/* WASI has no file modes: a file has the permissions that the host gives it,
   and the libc has no way to change them, so its chmod fails with ENOSYS.
   A program that copies, unpacks or creates files sets a mode as a matter of
   course (the tar package does so for each member of an archive), and the
   failure would stop it for a mode that nothing can read back. The link
   wraps chmod and fchmod, so that a call reaches these two functions. They
   succeed and leave the file as it is. The libc defines both in one object
   with the rest of its file calls, so a definition of the same name would
   clash with it. */
// NOLINTBEGIN(bugprone-reserved-identifier)
int __wrap_chmod(const char *path, unsigned int mode) {
  (void)path;
  (void)mode;
  return 0;
}

int __wrap_fchmod(int descriptor, unsigned int mode) {
  (void)descriptor;
  (void)mode;
  return 0;
}
// NOLINTEND(bugprone-reserved-identifier)
