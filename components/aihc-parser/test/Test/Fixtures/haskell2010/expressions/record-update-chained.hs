module ExprS315RecordUpdateChained where
data R = R { a :: Int, b :: Int }
x r = r { a = 1 } { b = 2 }
