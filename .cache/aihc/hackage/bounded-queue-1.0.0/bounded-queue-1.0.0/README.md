# bounded-queue

This library provides a strict, immutable, thread-safe, single-ended, bounded
queue. When the insert limit is reached and a `cons` is attempted, this `BQueue`
automatically drops old entries off its end. Thus, writes always succeed and
never block.

This data structure is intended as a "sliding window" over some stream of data,
where we wish old entries to be naturally forgotten. Since this is an immutable
data structure and not a concurrent queue, we provide instances for the usual
useful typeclasses with which one can perform analysis over the entire "window".
