#if MIN_VERSION_haskeline(0,8,0)
#define MONADCONSTRAINT (MonadIO m, MonadMask m)
#else
#define MONADCONSTRAINT MonadException m
#endif
