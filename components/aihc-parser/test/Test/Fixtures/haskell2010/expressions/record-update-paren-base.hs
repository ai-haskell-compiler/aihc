module ExprS315RecordUpdateParenBase where
data R = R { a :: Int }
x r = (f r) { a = 1 }
