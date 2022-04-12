# VectorProvider

An F# library that generates dimension-typed vectors and arithmetic operators for them. This library is an erased type provider - during compilation all `Vector<_>` types are unwrapped to `array<int>` types. This gives compile-time checking of vector dimensions with the same runtime performance of non-length checked arrays.
```fs
open VectorProvider

type Vec3 = Vector<3u>
let pt3d = Vec3 (1, 2, 3)
let one = pt3d.Dimension1
pt3d.Dimension2 <- 5
let four = pt3d.Dimension4 // Compiler error FS0039: The type 'Vector<3>' does not define the field, constructor or member 'Dimension4'
pt3d.Dimension4 <- 7 // Compiler error FS0039: The type 'Vector<3>' does not define the field, constructor or member 'Dimension4'

type Vec4 = Vector<4u>
let pt4dError = Vec4 (1, 2, 3) // Compiler error FS0505: The member or object constructor 'Vector,dimension="4"' does not take 3 arguments
let pt4d = Vec4 (0, 0, 0, 0)
pt4d.Dimension4 <- 1

let pt1 = Vec3 (1, 2, 3)
let pt2 = Vec3 (4, 5, 6)
let pt3 = pt1 +. pt2 // = Vec3 (5, 7, 9)
let pt4 = pt2 -. pt1 // = Vec3 (3, 3, 3)
let pt5 = -.pt1 // = Vec3 (-1, -2, -3)

let pt6 = pt3d +. pt4d // Compiler error FS0001: This expression was expected to have type 'Vector<3>' but here has type 'Vector<4>'
```

## To do
- Add more vector arithmetic operators
- Generalize to types other than `int`
  - This *may* require [RFC FS-1023](https://github.com/fsharp/fslang-design/blob/main/RFCs/FS-1023-type-providers-generate-types-from-types.md) to implement in a generalized way
- Improve testing
  - FsCheck views a `Vector<_>` as the type after erasure (`array<int>`), so property-based testing via FsCheck passes in arrays that are not length-checked. For now FsCheck is used to generate `int * int * int` values, which can be piped to the `Vector<3u>` constructor to create test data
  - The current tests only involve `Vector<3u>` - is it possible to have a bug in `Vector<n>` but not `Vector<m>`?
