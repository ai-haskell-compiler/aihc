# flush-queue

Two bounded blocking queues, one for `STM` another for `IO`, which are optimized for taking many
elements at the same time, instead of popping individual ones of the queue.
