#define CCALL(name,signature) \
foreign import ccall unsafe #name \
    c_##name :: signature

CCALL(foo, Int -> IO ())
