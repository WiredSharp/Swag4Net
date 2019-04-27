﻿open System
open System.IO

open System.Net.Http
open Argu
open Swag4Net.Core
open Swag4Net.Generators.RoslynGenerator
open CsharpGenerator
open CodeGeneration
open System.IO
open Swag4Net.Core

let (/>) a b =
    Path.Combine(a, b)

type CLIArguments =
    | [<Mandatory>] SpecFile of path:string
    | Namespace of string
    | ClientName of string
    | [<Mandatory>] OutputFolder of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | SpecFile _ -> "specify a Swagger spec file."
            | Namespace _ -> "specify namespace of generated code."
            | ClientName _ -> "specify client name."
            | OutputFolder _ -> "specifyoOutput folder."

let getRawSpec (path:string) =
  try
    let uri = Uri path
    if uri.IsFile
    then File.ReadAllText path
    elif uri.IsAbsoluteUri
    then
      use http = new HttpClient()
      http.GetStringAsync uri |> fun t -> t.GetAwaiter().GetResult()
    else File.ReadAllText path
  with | _ -> 
    path |> Path.GetFullPath |> File.ReadAllText

[<EntryPoint>]
let main argv =
  
  let parser = ArgumentParser.Create<CLIArguments>()
  try
    let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
    let specFile = results.GetResult <@ SpecFile @>
    let ns = results.GetResult(<@ Namespace @>, defaultValue="GeneratedClient")
    let clientName = results.GetResult(<@ ClientName @>, defaultValue="ApiClient")
    let outputFolder = results.GetResult <@ OutputFolder @>
    
    let http = new HttpClient()
    
    let parseSpec =
      if Path.GetExtension specFile = "yaml"
      then YamlParser.parseSwagger
      else JsonParser.parseSwagger
    
    let swagger = specFile |> getRawSpec |> parseSpec http
  
    let settings =
      { Namespace=ns }
    
    let client = generateClients settings swagger clientName
    
    let dtos =
      generateDtos settings swagger.Definitions
    
    outputFolder |> Directory.CreateDirectory |> ignore
    
    let csFilePath = outputFolder /> "Dtos.cs"
    File.WriteAllText(csFilePath, dtos)
    
    let csFilePath = outputFolder /> "Client.cs"
    File.WriteAllText(csFilePath, client)

  with e ->
      printfn "%s" e.Message

  0 // return an integer exit code
