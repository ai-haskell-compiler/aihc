## 0.2.0.1 (Feb. 2018)

* Fixed a foldr vs. foldl bug with algorithmic implications

## 1.0.0 (June 2022)

* Changed input type from `HashMap` to `Map`
* Removed superfluous internal map lookups by threading values alongside keys throughout the algorithm
* Used newtypes internally for algorithmic clarity
