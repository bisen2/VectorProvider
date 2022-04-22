namespace VectorProvider

module MathNetVector =
  open FSharp.Quotations
  open ProviderImplementation.ProvidedTypes
  open MathNet.Numerics.LinearAlgebra

  module private GeneralHelpers =

    let createOriginCtor (typ: ProvidedTypeDefinition) (dim: uint) =
      let invocation =
        <@@ Vector<int>.Build.Dense(int dim) @@>
      let ctor = ProvidedConstructor (parameters = [], invokeCode = fun _ -> invocation)
      do
        ctor.AddXmlDoc "Creates a vector pointing to the origin"
        typ.AddMember ctor

    let createPointCtor (typ: ProvidedTypeDefinition) (dim: uint) =
      let invocation (args: list<Expr>) =
        List.init (int dim) (fun i -> <@@ %%(args[i]): int @@>)
        |> fun x -> Expr.NewArray(typeof<int>, x)
        |> fun x -> <@@ Vector<int>.Build.DenseOfArray(%%x) @@>
      let ctor =
        ProvidedConstructor (
          parameters = [ for i in 1u .. dim -> ProvidedParameter ($"Dimension{i}", typeof<int>) ],
          invokeCode = invocation )
      do
        ctor.AddXmlDoc "Creates a vector with the given components"
        typ.AddMember ctor

  module private ArithematicHelpers =
    let x = 0

  open GeneralHelpers
  open ArithematicHelpers

  let private createCtors typ dim = ()

  let private createDimensionProperties typ dim = ()

  let private createArithmeticMethods typ dim = ()

  let createTypeDefinition asm ns typeName dim =
    let typ = ProvidedTypeDefinition (asm, ns, typeName, Some typeof<Vector<int>>)
    do
      createCtors typ dim
      createDimensionProperties typ dim
      createArithmeticMethods typ dim
    typ
