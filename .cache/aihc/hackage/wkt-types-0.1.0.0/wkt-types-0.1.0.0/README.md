# wkt-types

This library implements parsers for `.wkt` files, based on the specification at https://www.ogc.org/standard/sfa/. For lack of clarity, some geometries (triangles/TIN/polyhedralSurfaces) aren't completely implemented yet. 

## How to use

`Data.WTK.IO` exports two key methods, `readWKTFile` and `writeWKTFile`. When read, a `.wkt` file will be interpreted into a `Geometries` object. You can manipulate it at will, and then use `writeWKTFile` to write the `.wkt` file for the new `Geometries`. 
Individual types such as `Point`, `LineString`, `Polygon`, etc. also have their own individual parsers, serializers (`fromWKT`) and deserializers (`toWKT`).

Geometries are always read with `Double` as its base type for simplicity, but can be changed through `fmap`ing. 