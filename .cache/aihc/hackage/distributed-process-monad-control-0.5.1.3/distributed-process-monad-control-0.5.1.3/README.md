### distributed-process-monad-control

Orphan instances for `MonadBase` and `MonadBaseControl`. Please see the [tutorial](http://haskell-distributed.github.io/tutorials/3ch.html#monad-transformer-stacks) for an introduction and use case for these instances. Also please note that these instances enable the use of functions that are un-sound in the context of `distributed-process`. Functions such as `forkIO` (or, `fork` from `lifted-base`) compromise invariants in the Process monad and can lead to confusing and subtle issues. Always use the Cloud Haskell functions such as `spawnLocal` instead.

This repository is part of Cloud Haskell.

See http://haskell-distributed.github.io for documentation, user guides,
tutorials and assistance.

### Getting Help / Raising Issues

Please visit our [bug tracker](http://cloud-haskell.atlassian.net) to submit
issues. Anyone can browse, although you'll need to provide an email address
and create an account in order to submit new issues.

If you'd like to talk to a human, please contact us at the parallel-haskell
mailing list in the first instance - parallel-haskell@googlegroups.com.

### License

distributed-process-monad-control is made available under a BSD-3 license.
