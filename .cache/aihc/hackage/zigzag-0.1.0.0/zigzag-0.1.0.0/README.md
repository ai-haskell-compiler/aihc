# zigzag

[Zig-Zag encoding](https://developers.google.com/protocol-buffers/docs/encoding#signed-ints) of integers into natural numbers.
This encoding scheme has the advantage that LEB128, which is normally only specified for unsigned integers, will naturally represent small-magnitude signed integers (positive or negative) in few bytes.
