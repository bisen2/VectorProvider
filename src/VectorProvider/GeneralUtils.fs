namespace VectorProvider

module private GeneralUtils =

  open FSharp.Quotations
  open ProviderImplementation.ProvidedTypes

  module private Helpers =

    let createOriginCtor (typ: ProvidedTypeDefinition) (dim: uint) =
      let invocation =
        List.init (int dim) (fun _ -> <@@ 0 @@>)
        |> fun x -> Expr.NewArray (typeof<int>, x)
      let ctor = ProvidedConstructor (parameters = [], invokeCode = fun _ -> invocation)
      do
        ctor.AddXmlDoc "Creates a vector pointing to the origin"
        typ.AddMember ctor

    let createPointCtor (typ: ProvidedTypeDefinition) (dim: uint) =
      let invocation (args: list<Expr>)=
        List.init (int dim) (fun i -> <@@ %%(args[i]): int @@>)
        |> fun x -> Expr.NewArray (typeof<int>, x)
      let ctor =
        ProvidedConstructor (
          parameters = [ for i in 1u .. dim -> ProvidedParameter ($"Dimension{i}", typeof<int>) ],
          invokeCode = invocation )
      do
        ctor.AddXmlDoc "Creates a vector with the given components"
        typ.AddMember ctor

    let createDimProp (typ: ProvidedTypeDefinition) (dim: uint) =
      let prop =
        ProvidedProperty (
          propertyName = $"Dimension{dim + 1u}",
          propertyType = typeof<int>,
          getterCode = (fun args -> <@@ (%%(args[0]): array<int>)[int dim] @@>),
          setterCode = (fun args -> <@@ (%%(args[0]): array<int>)[int dim] <- (%%(args[1]): int) @@>) )
      do
        prop.AddXmlDoc $"The dimension {dim + 1u} component of the vector"
        typ.AddMember prop

  open Helpers

  let createCtors (typ: ProvidedTypeDefinition) (dim: uint) =
    [ createOriginCtor; createPointCtor ]
    |> List.iter (fun f -> f typ dim)

  let createDimensionProperties (typ: ProvidedTypeDefinition) (dim: uint) =
    [ 0u .. dim - 1u ]
    |> List.iter (createDimProp typ)
