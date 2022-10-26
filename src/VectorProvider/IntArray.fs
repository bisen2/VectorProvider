namespace VectorProvider

/// Logic for the specifics of the provider using the `array<int>` backing type
module IntArray =
  open FSharp.Quotations
  open Implementation

  module private Helpers =

    let originCtor maxDim _ =
      List.init (int maxDim) (fun _ -> <@@ 0 @@>)
      |> fun x -> Expr.NewArray(typeof<int>, x)

    let pointCtor maxDim (args: list<Expr>) =
      List.init (int maxDim) (fun i -> <@@ %%(args[i]):int @@>)
      |> fun x -> Expr.NewArray(typeof<int>, x)

    let dimPropGet maxDim thisDim (args: list<Expr>) =
      <@@ (%%(args[0]):array<int>)[int thisDim] @@>

    let dimPropSet maxDim thisDim (args: list<Expr>) =
      <@@ (%%(args[0]):array<int>)[int thisDim] <- (%%(args[1]):int) @@>

    let negateFunc maxDim (args: list<Expr>) =
      List.init (int maxDim) (fun i -> <@@ -(((%%args[0]):array<int>)[int i]) @@>)
      |> fun x -> Expr.NewArray(typeof<int>, x)

    let additionFunc maxDim (args: list<Expr>) =
      List.init (int maxDim) (fun i -> <@@ ((%%args[0]):array<int>)[int i] + ((%%args[1]):array<int>)[int i] @@>)
      |> fun x -> Expr.NewArray(typeof<int>, x)

    let subtractionFunc maxDim (args: list<Expr>) =
      List.init (int maxDim) (fun i -> <@@ ((%%args[0]):array<int>)[int i] - ((%%args[1]):array<int>)[int i] @@>)
      |> fun x -> Expr.NewArray(typeof<int>, x)

  open Helpers

  /// Creates a ProvidedTypeDefinition with the specified name and dimensionality
  let createTypeDefinition asm ns typeName dim =
    let baseType = Some typeof<array<int>>
    let invocations =
      { originCtor      = originCtor dim
        pointCtor       = pointCtor dim
        dimPropGet      = dimPropGet dim
        dimPropSet      = dimPropSet dim
        negateFunc      = negateFunc dim
        additionFunc    = additionFunc dim
        subtractionFunc = subtractionFunc dim }
    createTypeDefinition invocations baseType asm ns typeName dim
