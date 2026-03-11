# splitmix-distributions

[![Haskell CI](https://github.com/ocramz/splitmix-distributions/actions/workflows/ci.yaml/badge.svg)](https://github.com/ocramz/splitmix-distributions/actions/workflows/ci.yaml)

Random samplers for some common distributions, as well as a convenient interface for composing them, based on `splitmix`.


## Usage

Compose your random sampler out of simpler ones thanks to the Applicative and Monad interface, e.g. this is how you would declare and sample a binary mixture of Gaussian random variables:


    import Control.Monad (replicateM)
    import System.Random.SplitMix.Distributions (Gen, sample, bernoulli, normal)

    process :: Gen Double
    process = do
        coin <- bernoulli 0.7
        if coin
        then
            normal 0 2
        else
            normal 3 1

    dataset :: [Double]
    dataset = sample 1234 $ replicateM 20 process


and sample your data in a pure (`sample`) or monadic (`sampleT`) setting.

## Implementation details

The library is built on top of `splitmix`, so the caveats on safety and performance that apply there are relevant here as well.
