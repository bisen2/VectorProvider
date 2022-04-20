namespace VectorProvider

open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

/// Choice of backing type used by the provider. This determines what type the vector is erased to at runtime
[<RequireQualifiedAccess>]
type BackingType =
  | IntArray = 1

[<TypeProvider>]
type VectorProvider(config) as this =
  inherit TypeProviderForNamespaces(config)

  let asm = System.Reflection.Assembly.GetExecutingAssembly()
  let ns = "VectorProvider"
  let baseType = Some typeof<array<int>>
  let staticParams =
    [ ProvidedStaticParameter ("dimension", typeof<uint>)
      ProvidedStaticParameter ("backingType", typeof<BackingType>, Some BackingType.IntArray) ]

  let vecType = ProvidedTypeDefinition (asm, ns, "Vector", None)

  do
    vecType.DefineStaticParameters (
      staticParams,
      fun typeName -> function
        | [| :? uint as dim; :? int as bt |] ->
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
