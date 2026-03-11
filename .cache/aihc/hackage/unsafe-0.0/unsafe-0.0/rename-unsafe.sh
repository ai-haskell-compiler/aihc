darcs-replace-rec unsafePerformIO Unsafe.performIO $*
darcs-replace-rec unsafeInterleaveIO Unsafe.interleaveIO $*
darcs-replace-rec unsafeInterleaveST Unsafe.interleaveST $*
darcs-replace-rec unsafeIOToST Unsafe.ioToST $*
darcs-replace-rec unsafeSTToIO Unsafe.stToIO $*
darcs-replace-rec unsafeForeignPtrToPtr Unsafe.foreignPtrToPtr $*
darcs-replace-rec unsafeCoerce Unsafe.coerce $*
