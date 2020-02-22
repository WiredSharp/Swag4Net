open System.Net

#load "../Swag4Net.Core/Domain/ApiModel.fs"

#load "Helpers.fs"
#load "DiffTool.fs"

open System
open Swag4Net.DiffTool.Comparer
open Swag4Net.DiffTool.Helpers
open Swag4Net.Core.Domain
open Swag4Net.Core.Domain.ApiModel

//
//let s1 = Seq.init 5 (fun x -> x)
//let s2 = seq { for i in 6..10 -> i }
//
//let s3 = seq { for i in 16..30 -> i }
//
//let l2 = [ for i in 6..10 -> i ]
//
//let l3 = [ for i in 7..12 -> i ]
//
//let full = Seq.append s1 (Seq.append s2 s3)
//
//let all = [s1; s2; s3]
//
//let flatten sequences =
//  Seq.collect (fun x -> x) sequences
//  
//let compInt i1 i2 =
//  if i1 > i2 then
//    1
//  else if i1 = i2 then
//    0
//  else
//    -1
//
//let capitals =
//    [("Australia", "Canberra"); ("Canada", "Ottawa"); ("China", "Beijing");
//        ("Denmark", "Copenhagen"); ("Egypt", "Cairo"); ("Finland", "Helsinki");
//        ("France", "Paris"); ("Germany", "Berlin"); ("India", "New Delhi");
//        ("Japan", "Tokyo"); ("Mexico", "Mexico City"); ("Russia", "Moscow");
//        ("Slovenia", "Ljubljana"); ("Spain", "Madrid"); ("Sweden", "Stockholm");
//        ("Taiwan", "Taipei"); ("Hong Kong", "Hong Kong");("USA", "Washington D.C.")]
//    |> Map.ofList
//    
//let capitals2 =
//    [("Australia", "Canberra"); ("Canada", "Ottawa"); ("China", "Beijing");
//        ("Denmark", "Copenhagen"); ("Egypt", "Cairo"); ("Finland", "Helsinki");
//        ("France", "Paris"); ("Germany", "Berline"); ("India", "New Delhi");
//        ("Japan", "Tokyo"); ("Mexico", "Mexico City"); ("Russia", "Moscow");
//        ("Spain", "Madrid"); ("Sweden", "Stockholm");
//        ("Taiwan", "Taipei"); ("Hong Kong", "Hong Kong");("USA", "Washington D.C.")]
//    |> Map.ofList
//
//let compareString k1 k2 =
//  String.Compare(k1, k2, StringComparison.OrdinalIgnoreCase)
//
//let diffString path level previous actual =
//    if (not (iEqual previous actual)) then
//        seq { yield Modified {Path = path; Previous=previous; Actual=actual; Level = level } }
//    else
//        Seq.empty
//            
//MapHelpers.syncZip compareString capitals capitals2 |> Seq.iter (fun item -> printfn "%A" item)
//
//ListHelpers.syncZip compInt l2 l3 |> Seq.iter (fun item -> printfn "%A" item)
//
//Comparer.compareMap iCompare diffString (fun x -> x) ["aPath"] DiffLevel.Info capitals Map.empty |> Seq.iter (fun item -> printfn "%A" item)

let emptyApi = { 
   BasePath = ""
   Infos = { 
       Description = ""
       Version = ""
       Title = ""
       TermsOfService = ""
       Contact = None
       License = None
   } 
   Paths = []
   Definitions = []
 }

let simpleInfoApi = { emptyApi with 
                       Infos = { 
                           Description = "previous description"
                           Version = "1.0.0"
                           Title = "previous API"
                           TermsOfService = ""
                           Contact = Some (Email "john@doe.com")
                           License = Some { Name="mylicense"; Url="http://mylicense.com" }
                       } 
                     }

let apiOnePath = { simpleInfoApi
                    with Paths = [
                                   {
                                       Path = "/orders"
                                       Verb = "GET"
                                       Tags = []
                                       Summary = "retrieve orders"
                                       Description = "retrieve a summary for all orders"
                                       OperationId = "get_orders"
                                       Consumes = ["application/json"]
                                       Produces = ["application/json"]
                                       Parameters = []
                                       Responses = [{
                                           Code = StatusCode HttpStatusCode.OK;
                                           Description = "success"
                                           Type = None
                                       }]               
                                   }
                               ]}

let previousApi = apiOnePath
let currentApi = { apiOnePath with Paths = [ {
                                                apiOnePath.Paths.[0] with
                                                    Consumes = ["application/xml"]
                                             }
                                           ]
                }

//let result = Compare previousApi currentApi

Compare previousApi currentApi |> Seq.iter (fun x -> printfn "%A" x)