module Huffman where

-- A Huffman codec, slightly adapted from Bird
-- (with properties added)

data BTree a = Leaf a | Fork (BTree a) (BTree a)
  deriving Show

decode t bs = if null bs then [] else dec t t bs

dec (Leaf x) t bs = x : decode t bs
dec (Fork xt yt) t (b:bs) = dec (if b then yt else xt) t bs

encode t cs = enc (codetable t) cs

enc table [] = []
enc table (c:cs) = (table ! c) ++ enc table cs

((x, bs) : xbs) ! y = if x == y then bs else xbs ! y

codetable t = tab [] t

tab p (Leaf x) = [(x,p)]
tab p (Fork xt yt) = tab (p++[False]) xt ++ tab (p++[True]) yt

collate [] = []
collate (c:cs) = insert (1+n, Leaf c) (collate ds)
  where (n, ds) = count c cs

count x [] = (0, [])
count x (y:ys) = if x == y then (1+n, zs) else (n, y:zs)
  where (n, zs) = count x ys

insert (w, x) [] = [(w, x)]
insert (w0, x) ((w1, y):wys)
  | w0 <= w1 = (w0, x) : (w1, y) : wys
  | otherwise = (w1, y) : insert (w0, x) wys

hufftree cs = mkHuff (collate cs)

mkHuff [(w, t)] = t
mkHuff ((w0, t0):(w1, t1):wts) =
  mkHuff (insert (w0+w1, Fork t0 t1) wts)

-- Properties

infixr 0 -->
False --> _ = True
True --> x = x

prop_decEnc cs = length h > 1 --> (decode t (encode t cs) == cs)
  where
    h = collate cs
    t = mkHuff h
    types = cs :: String

prop_optimal (cs, t) =
    t `treeOf` h --> cost h t >= cost h (mkHuff h)
  where
    h = collate cs
    types = cs :: String

-- Cost

cost h t = cost' h (codetable t)

cost' h [] = 0
cost' h ((c, bs):cbs) = (n * length bs) + cost' h cbs
  where
    n = head [n | (n, Leaf sym) <- h, sym == c]

leaves (Leaf c) = [c]
leaves (Fork xt yt) = leaves xt ++ leaves yt

treeOf t h = leaves t === [c | (_, Leaf c) <- h]

[] === [] = True
(x:xs) === ys = case del x ys of
                  Nothing -> False
                  Just zs -> xs === zs
_ === _ = False

del x [] = Nothing
del x (y:ys) = if x == y then Just ys else case del x ys of
                                             Nothing -> Nothing
                                             Just zs -> Just (y:zs)
