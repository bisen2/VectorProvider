module ArithmeticTests

open Expecto
open VectorProvider

module private Vec3 =
  open TestingUtils.Vec3

  let negation (v: Vector<3u>) =
    test $"Negation negates each component for vector %A{v}" {
      Expect.equal (-.v).Dimension1 (v.Dimension1 * -1) "Dimension1 of -v should be -1 * v.Dimension1"
      Expect.equal (-.v).Dimension2 (v.Dimension2 * -1) "Dimension2 of -v should be -1 * v.Dimension2"
      Expect.equal (-.v).Dimension3 (v.Dimension3 * -1) "Dimension3 of -v should be -1 * v.Dimension3"
    }

  let addition (v1: Vector<3u>, v2: Vector<3u>) =
    test $"Addition adds each component for vectors %A{v1} and %A{v2}" {
      Expect.equal (v1 +. v2).Dimension1 ((v1.Dimension1) + (v2.Dimension1)) "Dimension1 of v1 + v2 should be v1.Dimension1 + v2.Dimension1"
      Expect.equal (v1 +. v2).Dimension2 ((v1.Dimension2) + (v2.Dimension2)) "Dimension2 of v1 + v2 should be v1.Dimension2 + v2.Dimension2"
      Expect.equal (v1 +. v2).Dimension3 ((v1.Dimension3) + (v2.Dimension3)) "Dimension3 of v1 + v2 should be v1.Dimension3 + v2.Dimension3"
    }

  let subtraction (v1: Vector<3u>, v2: Vector<3u>) =
    test $"Subtraction subtracts each component for vectors %A{v1} and %A{v2}" {
      Expect.equal (v1 -. v2).Dimension1 ((v1.Dimension1) - (v2.Dimension1)) "Dimension1 of v1 - v2 should be v1.Dimension1 - v2.Dimension1"
      Expect.equal (v1 -. v2).Dimension2 ((v1.Dimension2) - (v2.Dimension2)) "Dimension2 of v1 - v2 should be v1.Dimension2 - v2.Dimension2"
      Expect.equal (v1 -. v2).Dimension3 ((v1.Dimension3) - (v2.Dimension3)) "Dimension3 of v1 - v2 should be v1.Dimension3 - v2.Dimension3"
    }

  let allTests =
    testList "Arithmetic tests" [
      yield! randVectors() |> Seq.map negation
      yield! randVectorPairs() |> Seq.map addition
      yield! randVectorPairs() |> Seq.map subtraction
    ]

let allTests =
  testList "All arithmetic tests" [
    Vec3.allTests
  ]
