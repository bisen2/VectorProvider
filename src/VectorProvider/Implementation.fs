namespace VectorProvider

/// Base logic for creating the provider with the specifics of backing type abstracted away
module Implementation =
  open FSharp.Quotations
  open ProviderImplementation.ProvidedTypes

  /// A function that can be used as the invokeCode of a method
  type Invocation = list<Expr> -> Expr

  /// A set of invocations that can be used to create an implementation of the provider
  type InvocationImplementations =
    { originCtor      : Invocation
      pointCtor       : Invocation
      dimPropGet      : uint -> Invocation
      dimPropSet      : uint -> Invocation
      negateFunc      : Invocation
      additionFunc    : Invocation
      subtractionFunc : Invocation }

  module private Helpers =

    let private originCtor invokeCode (typ: ProvidedTypeDefinition) (dim: uint) =
      let ctor = ProvidedConstructor(parameters = [], invokeCode = invokeCode)
      do
        ctor.AddXmlDoc "Creates a vector pointing to the origin"
        typ.AddMember ctor

    let private pointCtor invokeCode (typ: ProvidedTypeDefinition) (dim: uint) =
      let ctor =
        ProvidedConstructor(
          parameters = [ for i in 1u .. dim -> ProvidedParameter($"Dimension{i}", typeof<int>) ],
          invokeCode = invokeCode )
      do
        ctor.AddXmlDoc "Creates a vector with the given components"
        typ.AddMember ctor

    let private dimProp getterCode setterCode (typ: ProvidedTypeDefinition) (dim: uint) =
      let prop =
        ProvidedProperty(
          propertyName = $"Dimension{dim + 1u}",
          propertyType = typeof<int>,
          getterCode = getterCode,
          setterCode = setterCode )
      do
        prop.AddXmlDoc $"The dimension {dim + 1u} component of the vector"
        typ.AddMember prop

    let private negationFunction invokeCode (typ: ProvidedTypeDefinition) (dim: uint) =
      let method =
        ProvidedMethod(
          methodName = "Negate",
          parameters = [ ProvidedParameter("vec", typ.AsType()) ],
          returnType = typ.AsType(),
          invokeCode = invokeCode,
          isStatic = true )
      do
        method.AddXmlDoc "Negates the given vector"
        typ.AddMember method

    let private additionFunction invokeCode (typ: ProvidedTypeDefinition) (dim: uint) =
      let method =
        ProvidedMethod(
          methodName = "Add",
          parameters =
            [ ProvidedParameter("vec1", typ.AsType())
              ProvidedParameter("vec2", typ.AsType()) ],
          returnType = typ.AsType(),
          invokeCode = invokeCode,
          isStatic = true )
      do
        method.AddXmlDoc "Adds the given vectors"
        typ.AddMember method

    let private subtractionFunction invokeCode (typ: ProvidedTypeDefinition) (dim: uint) =
      let method =
        ProvidedMethod(
          methodName = "Subtract",
          parameters =
            [ ProvidedParameter("vec1", typ.AsType())
              ProvidedParameter("vec2", typ.AsType()) ],
          returnType = typ.AsType(),
          invokeCode = invokeCode,
          isStatic = true )
      do
        method.AddXmlDoc "Subtracts the given vectors"
        typ.AddMember method

    /// Creates the constructors and dimension properties based on the given invocation code
    let generalMembers (invocations: InvocationImplementations) typ dim =
      originCtor invocations.originCtor typ dim
      pointCtor invocations.pointCtor typ dim
      [ 0u .. dim - 1u ]
      |> List.iter (fun i -> dimProp (invocations.dimPropGet i) (invocations.dimPropSet i) typ i)

    /// Creates the arithmetic functions based on the given invocation code
    let arithmeticMembers (invocation: InvocationImplementations) typ dim =
      negationFunction invocation.negateFunc typ dim
      additionFunction invocation.additionFunc typ dim
      subtractionFunction invocation.subtractionFunc typ dim

  /// Given the base type specific logic, generates a ProvidedTypeDefinition with the specified backing type and logic
  let createTypeDefinition (invocations: InvocationImplementations) (baseType: option<System.Type>) asm ns typeName dim =
    let typ = ProvidedTypeDefinition(asm, ns, typeName, baseType)
    Helpers.generalMembers invocations typ dim
    Helpers.arithmeticMembers invocations typ dim
    typ
