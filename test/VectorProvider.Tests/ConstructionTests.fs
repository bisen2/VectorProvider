module ConstructionTests

open Expecto
open VectorProvider

module private Vec3 =
  open TestingUtils.Vec3

  let defaultAllZeroes =
    test "Default ctor creates vector with all zeroes" {
      let v = Vec3()
      Expect.equal v.Dimension1 0 "Dimension1 should be 0"
      Expect.equal v.Dimension2 0 "Dimension2 should be 0"
      Expect.equal v.Dimension3 0 "Dimension3 should be 0"
    }

  let ctorSetsDims  (v: Vector<3u>)=
    test $"Non-default ctor should set dimensions for vector %A{v}" {
      let v = Vec3(1, 2, 3)
      Expect.equal v.Dimension1 1 "Dimension1 should be 1"
      Expect.equal v.Dimension2 2 "Dimension2 should be 2"
      Expect.equal v.Dimension3 3 "Dimension3 should be 3"
    }

  let dimensionsAreSettable (vSet: Vector<3u>) =
    test $"All dimensions are settable to vector %A{vSet}" {
      let v = Vec3()
      Expect.equal v.Dimension1 0 "Dimension1 should be 0"
      Expect.equal v.Dimension2 0 "Dimension2 should be 0"
      Expect.equal v.Dimension3 0 "Dimension3 should be 0"
      v.Dimension1 <- vSet.Dimension1
      v.Dimension2 <- vSet.Dimension2
      v.Dimension3 <- vSet.Dimension3
      Expect.equal v.Dimension1 vSet.Dimension1 $"Dimension1 should be {vSet.Dimension1}"
      Expect.equal v.Dimension2 vSet.Dimension2 $"Dimension2 should be {vSet.Dimension2}"
      Expect.equal v.Dimension3 vSet.Dimension3 $"Dimension3 should be {vSet.Dimension3}"
    }

  let allTests =
    testList "Vec3 tests" [
      defaultAllZeroes
      yield! randVectors() |> Seq.map dimensionsAreSettable
      yield! randVectors() |> Seq.map ctorSetsDims
    ]

let allTests =
  testList "Construction tests" [
    Vec3.allTests
  ]
