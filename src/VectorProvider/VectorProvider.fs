namespace VectorProvider

open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

[<TypeProvider>]
type VectorProvider(config) as this =
  inherit TypeProviderForNamespaces(config)

  let asm = System.Reflection.Assembly.GetExecutingAssembly()
  let ns = "VectorProvider"
  let baseType = Some typeof<array<int>>
  let staticParams = [ ProvidedStaticParameter ("dimension", typeof<uint>) ]

  let vecType = ProvidedTypeDefinition (asm, ns, "Vector", baseType)

  do
    vecType.DefineStaticParameters (
      staticParams,
      fun typeName -> function
        | [| :? uint as dim |] ->
            let typ = ProvidedTypeDefinition (asm, ns, typeName, baseType)
            do
              GeneralUtils.createCtors typ dim
              GeneralUtils.createDimensionProperties typ dim
              ArithmeticUtils.createArithmeticMethods typ dim
            typ
        | _ -> failwith "Invalid static parameters" )

  do this.AddNamespace (ns, [vecType])

[<assembly: TypeProviderAssembly>]
do ()
