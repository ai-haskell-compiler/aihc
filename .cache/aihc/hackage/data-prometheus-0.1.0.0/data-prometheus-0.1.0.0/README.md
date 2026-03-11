# data-prometheus

Pure Prometheus metrics parser and builder.

## Usage

### Parsing metrics

```haskell
import qualified Data.Prometheus
import qualified Network.Wreq
import qualified Data.ByteString.Lazy

main :: IO ()
main = do
  r <- Network.Wreq.get "http://localhost:9100/metrics"
  case Data.Prometheus.parseProm
        (Data.ByteString.Lazy.toStrict $ r ^. responseBody)
  of
    Right result -> print result
    Left err -> putStrLn err
```

### Generating metrics

In monadic manner

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Prometheus
import qualified Data.Text.IO

main :: IO ()
main = do
  Data.Text.IO.putStrLn
  $ runMetrics (metric "readme")
  $ do
    addMetric
      "subMetric"
      (Counter 13)

    logError "something is not right"

    addMetric'
      ( sub "anotherSubMetric"
      . sub "gauge"
      . label "key" "val")
      (Gauge 13)
```

or alternatively define `ToMetrics` instances for your data types.

Above example will output:

```
# HELP readme_gauge_anotherSubMetric 
# TYPE readme_gauge_anotherSubMetric gauge
readme_gauge_anotherSubMetric{key="val"} 13.0
# HELP readme_subMetric 
# TYPE readme_subMetric counter
readme_subMetric 13.0
# ERROR something is not right
```
