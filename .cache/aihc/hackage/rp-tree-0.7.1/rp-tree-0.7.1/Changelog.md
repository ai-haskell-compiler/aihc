0.7

- add batch index creation 'treeBatch', 'forestBatch', 'dataBatch' and corresponding unit tests (#5)

0.6

- add 'knnPQ' based on 'heaps', to be confirmed how efficient it is

0.5.1

- no need for 'microlens' and 'hspec' in the library

0.5

- fixed intermittent bug in 'knn', originally due to messing up the order of 'take' and 'sort'.

0.4

- add function to compute the forest construction parameters from the dataset dimensions (rpTreeCfg)
- add type parameter to RPTree to label tree branching points

0.3.6

- relax lower dependency bounds to ensure it builds with GHC 8.6.5 (== stackage lts 14.27) as well

0.2

- fix 'candidates' such that 'knn' now does the right thing
- now 'knn' accepts a distance function as parameter as well
- add I/O functionality
- some time benchmarks
