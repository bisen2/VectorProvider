namespace VectorProvider

module private ArithmeticUtils =

  open FSharp.Quotations
  open ProviderImplementation.ProvidedTypes

  module private Helpers =

    let createNegateFunction (typ: ProvidedTypeDefinition) (dim: uint) =
      let invocation (args: list<Expr>) =
        List.init (int dim) (fun i -> <@@ -(((%%args[0]): array<int>)[int i]) @@>)
        |> fun x -> Expr.NewArray (typeof<int>, x)
      let method =
        ProvidedMethod (
          methodName = "Negate",
          parameters = [ ProvidedParameter ("vec", typ.AsType()) ],
          returnType = typ.AsType(),
          invokeCode = invocation,
          isStatic = true )
      do
        method.AddXmlDoc "Negates the given vector"
        typ.AddMember method

    let createAdditionFunction (typ: ProvidedTypeDefinition) (dim: uint) =
      let invocation (args: list<Expr>) =
        List.init (int dim) (fun i -> <@@ ((%%args[0]): array<int>)[int i] + ((%%args[1]): array<int>)[int i] @@>)
        |> fun x -> Expr.NewArray (typeof<int>, x)
      let method =
        ProvidedMethod (
          methodName = "Add",
          parameters =
            [ ProvidedParameter ("vec1", typ.AsType())
              ProvidedParameter ("vec2", typ.AsType()) ],
          returnType = typ.AsType(),
          invokeCode = invocation,
          isStatic = true )
      do
        method.AddXmlDoc "Adds the given vectors"
        typ.AddMember method

    let createSubtractionFunction (typ: ProvidedTypeDefinition) (dim: uint) =
      let invocation (args: list<Expr>) =
        List.init (int dim) (fun i -> <@@ ((%%args[0]): array<int>)[int i] - ((%%args[1]): array<int>)[int i] @@>)
        |> fun x -> Expr.NewArray (typeof<int>, x)
      let method =
        ProvidedMethod (
          methodName = "Subtract",
          parameters =
            [ ProvidedParameter ("vec1", typ.AsType())
              ProvidedParameter ("vec2", typ.AsType()) ],
          returnType = typ.AsType(),
          invokeCode = invocation,
          isStatic = true )
      do
        method.AddXmlDoc "Subtracts the given vectors"
        typ.AddMember method

  open Helpers

  let createArithmeticMethods (typ: ProvidedTypeDefinition) (dim: uint) =
    [ createNegateFunction; createAdditionFunction; createSubtractionFunction ]
    |> List.iter (fun f -> f typ dim)
