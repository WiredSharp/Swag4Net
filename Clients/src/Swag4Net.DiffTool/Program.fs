// Learn more about F# at http://fsharp.org

open System
open Swag4Net.DiffTool.Comparer
open Swag4Net.Core.Domain.ApiModel

[<EntryPoint>]
let main argv =
  let previous = { 
         BasePath = "/api/v1"
         Infos = { 
             Description = "my API description"
             Version = "1.0.0"
             Title = "test API"
             TermsOfService = ""
             Contact = Some (Email "john@doe.com")
             License = Some { Name="mylicense"; Url="http://mylicense.com" }
         } 
         Paths = []
         Definitions = []
       }

  let actual = { 
         BasePath = "/api/v2"
         Infos = { 
             Description = "my API description"
             Version = "2.0.0"
             Title = "test API"
             TermsOfService = ""
             Contact = Some (Email "john@doe.com")
             License = Some { Name="mylicense"; Url="http://mylicense.com" }
         } 
         Paths = []
         Definitions = []
       }

  Compare previous actual |> Seq.iter (fun x -> printfn "%A" x)
  0 // return an integer exit code
