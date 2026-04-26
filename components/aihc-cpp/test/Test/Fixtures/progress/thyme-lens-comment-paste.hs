#define LENS(S,F,A) {-# INLINE _/**/F #-}; _/**/F :: LensP S A; _/**/F = lens F $ \ S {..} F/**/_ -> S {F = F/**/_, ..}
LENS(Foo,bar,Baz)
LENS(Foo,bar,Baz{-comment-})
