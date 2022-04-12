namespace VectorProvider

[<AutoOpen>]
module VectorOperators =

  /// Negates the target vector
  let inline (~-.) (vec: ^T): ^T =
    (^T: (static member Negate: ^T -> ^T) vec)

  /// Adds the given vectors
  let inline (+.) (vec1: ^T) (vec2: ^T): ^T =
    (^T: (static member Add: ^T * ^T -> ^T) (vec1, vec2))

  let inline (-.) (vec1: ^T) (vec2: ^T): ^T =
    (^T: (static member Subtract: ^T * ^T -> ^T) (vec1, vec2))
