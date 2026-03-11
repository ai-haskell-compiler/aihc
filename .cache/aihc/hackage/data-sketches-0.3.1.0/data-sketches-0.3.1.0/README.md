# streaming-quantiles

The Business Challenge: Analyzing Big Data Quickly.

In the analysis of big data there are often problem queries that don’t scale because they require huge compute resources and time to generate exact results. Examples include count distinct, quantiles, most-frequent items, joins, matrix computations, and graph analysis.

If approximate results are acceptable, there is a class of specialized algorithms, called streaming algorithms, or sketches that can produce results orders-of magnitude faster and with mathematically proven error bounds. For interactive queries there may not be other viable alternatives, and in the case of real-time analysis, sketches are the only known solution.

For any system that needs to extract useful information from big data these sketches are a required toolkit that should be tightly integrated into their analysis capabilities. This technology has helped Yahoo (Verizon Media) successfully reduce data processing times from days or hours to minutes or seconds on a number of its internal platforms.

This project is dedicated to providing a broad selection of sketch algorithms of production quality. Contributions are welcome from those interested in further development of this science and art.

## Why use this project?

- Sketches are fast. The sketch algorithms in this library process data in a single pass and are suitable for both real-time and batch. Sketches enable streaming computation of set expression cardinalities, quantiles, frequency estimation and more. In addition, designing a system around sketching allows simplification of system's architecture and reduction in overall compute resources required for these heretofore difficult computation
- Built-in Theta Sketch set operators (Union, Intersection, Difference) produce sketches as a result (and not just a number) enabling full set expressions of cardinality, such as ((A ∪ B) ∩ (C ∪ D)) \ (E ∪ F). This capability along with predictable and superior accuracy (compared with Include/Exclude approaches) enable unprecedented analysis capabilities for fast queries.