namespace VectorProvider

module IntArray =
  open FSharp.Quotations
  open ProviderImplementation.ProvidedTypes

  module private GeneralHelpers =

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

  module private ArithmeticHelpers =

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

  open GeneralHelpers
  open ArithmeticHelpers

  let private createCtors typ dim =
    [ createOriginCtor; createPointCtor ]
    |> List.iter (fun f -> f typ dim)

  let private createDimensionProperties typ dim =
    [ 0u .. dim - 1u ]
    |> List.iter (fun i -> createDimProp typ i)

  let private createArithmeticMethods typ dim =
    [ createNegateFunction; createAdditionFunction; createSubtractionFunction ]
    |> List.iter (fun f -> f typ dim)

  let createTypeDefinition asm ns typeName dim =
    let typ = ProvidedTypeDefinition (asm, ns, typeName, Some typeof<array<int>>)
    do
      createCtors typ dim
      createDimensionProperties typ dim
      createArithmeticMethods typ dim
    typ
