module TestingUtils

open FsCheck
open VectorProvider

module Vec3 =

  type Vec3 = Vector<3u, BackingType.IntArray>

  let randVectors() =
    Arb.generate<int>
    |> Gen.three
    |> Gen.sample 1000 100
    |> Seq.map Vec3

  let randVectorPairs() =
    Seq.zip (randVectors()) (randVectors())
