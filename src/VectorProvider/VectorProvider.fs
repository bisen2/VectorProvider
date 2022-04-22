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
  let typeName = "VectorND"
  let staticParams =
    [ ProvidedStaticParameter("dimension", typeof<uint>)
      ProvidedStaticParameter("backingType", typeof<BackingType>, Some BackingType.IntArray) ]

  let vecType = ProvidedTypeDefinition (asm, ns, typeName, None)

  do
    vecType.DefineStaticParameters (
      staticParams,
      fun typeName -> function
        | [| :? uint as dim; :? int as bt |] when bt = int BackingType.IntArray ->
            IntArray.createTypeDefinition asm ns typeName dim
        | _ -> failwith "Invalid static parameters" )

  do this.AddNamespace (ns, [vecType])

[<assembly: TypeProviderAssembly>]
do ()
