#ifdef __wasm__
#error "this package is only for native targets"
#endif

long native_answer(void) { return 42; }
