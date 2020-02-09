﻿//#I "../../packages/netstandard.library/2.0.0/build/netstandard2.0/ref"
#r "netstandard"
#r "../../packages/newtonsoft.json/12.0.1/lib/netstandard2.0/newtonsoft.json.dll"
#r "../../packages/YamlDotNet/6.0.0/lib/netstandard1.3/YamlDotNet.dll"

#r "../../packages/Microsoft.CodeAnalysis.common/2.3.1/lib/netstandard1.3/Microsoft.CodeAnalysis.dll"
#r "../../packages/Microsoft.CodeAnalysis.CSharp/2.3.1/lib/netstandard1.3/Microsoft.CodeAnalysis.CSharp.dll"
#r "../../packages/microsoft.csharp/4.4.0/lib/netstandard2.0/Microsoft.CSharp.dll"
#r "../../packages/microsoft.codeanalysis.analyzers/2.6.1/analyzers/dotnet/cs/Microsoft.CodeAnalysis.Analyzers.dll"
#r "../../packages/microsoft.codeanalysis.analyzers/2.6.1/analyzers/dotnet/cs/Microsoft.CodeAnalysis.CSharp.Analyzers.dll"

#r "System.Net.Http.dll"

#load "Document.fs"
#load "Domain/SharedKernel.fs"
#load "Domain/OpenApiSpecification.fs"
#load "Domain/SwaggerSpecification.fs"
#load "Parsing.fs"
#load "v2/SwaggerParser.fs"
#load "v3/Parser.fs"

//#load "C:\dev\Swag4Net-1\Clients\src\Swag4Net.Generators.RoslynGenerator\RoslynDsl.fs"

open System
open System.IO
open Swag4Net.Core
open Document
open Swag4Net.Core.v3.Parser
open Swag4Net.Core.v3.Parser
open Parsing
//open Swag4Net.Generators.RoslynGenerator

let (/>) a b = Path.Combine(a, b)

let specv3File = __SOURCE_DIRECTORY__ /> ".." /> ".." /> "tests" /> "Assets" /> "openapiV3" /> "petstoreV3.yaml" |> File.ReadAllText

let doc = fromYaml specv3File

let parseComponents =
  parsing {
    let! schemas = 
      match doc |> selectToken "components.schemas" with
      | Some (SObject props) -> 
            parsing {
              let! r =
                props
                |> List.map (
                    fun (name, s) -> 
                      s
                      |> parseSchema
                      |> ParsingState.map (fun p -> name,Inlined p)
                    )
              return Some (Map r)
            } 
      | _ -> ParsingState.success None

    return 
      { Schemas=schemas
        Responses=None
        Parameters=None
        Examples=None
        RequestBodies=None
        Headers=None
        SecuritySchemes=None
        Links=None
        Callbacks=None }
  }

let spec = parseOpenApiDocument doc

// spec |> Result.map (fun r -> r.Components.Value.Schemas.Value.Item "ExtendedErrorModel")

spec |> Result.map (fun r -> r.Paths.Item "/pets")



//spec |> Result.map (fun r -> r.Paths.Item "/pets")

//spec
// |> Result.map (
//    fun r -> 
//      r.Paths
//      |> Map.toList
//      |> List.filter (fun (k,v) -> v.Get.Value.OperationId = "showPetById")
//    )

