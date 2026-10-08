module HiddenProduct (Span, makeSpan, spanWidth) where

-- | The export list hides the constructor. Another module can still unpack
-- this product into a strict field of its own constructor.
data Span = Span !Int !Int

makeSpan :: Int -> Int -> Span
makeSpan = Span

spanWidth :: Span -> Int
spanWidth (Span start end) = end - start
