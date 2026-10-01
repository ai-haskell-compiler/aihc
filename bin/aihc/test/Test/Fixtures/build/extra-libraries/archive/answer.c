/* The test compiles this file into lib/libanswer.a in a copy of the
   package. The package names the archive only in extra-libraries and
   extra-lib-dirs, so the link fails if the linker does not get them. */
int aihc_extra_libraries_answer(void) { return 42; }
