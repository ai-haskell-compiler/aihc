// An Objective-C source in cxx-sources, as in hfsevents. The implicit
// conversion from void * is correct C and Objective-C, but not C++. The
// cxx-options of the package select C++17, which Objective-C refuses.
int aihc_objc_twice(int value) {
  int storage = value;
  void *untyped = &storage;
  int *typed = untyped;
  return *typed * 2;
}
