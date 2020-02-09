namespace Swag4Net.Core.Tests

open Expecto
open Expecto.Logging

open System.Net
open Swag4Net.DiffTool.Comparer
open Swag4Net.Core.Domain.ApiModel

module ApiDiffTests =

    let logger = Log.create "Api diff tool"

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

    
    let tests =
        testList "specification diff tests" [
            test "empty api definition are equivalents" {
                let previousApi = emptyApi
                let currentApi = emptyApi
                let result = Compare previousApi currentApi
                Expect.isEmpty result "no diffItem should have been found"
            } 
            test "api definition with distinct basepath is notified" {
                let previousApi = { emptyApi with BasePath = "another base path" }
                let currentApi = emptyApi
                let result = Compare previousApi currentApi |> List.ofSeq
                Expect.hasLength (Infos result) 1 "changed basepath should be an info"
                Expect.isEmpty (Warnings result) "no warning should have been raised"
                Expect.isEmpty (Breakings result) "no breaking should have been raised"
            } 
            test "api definition with distinct infos/description is notified" {
                let previousApi = simpleInfoApi
                let currentApi = { simpleInfoApi with Infos = { 
                                                            simpleInfoApi.Infos with
                                                                Description = "new description"
                                                            }
                                }
                let result = Compare previousApi currentApi
                Expect.hasLength (Infos result) 1 "changed infos should be an info"
                Expect.isEmpty (Warnings result) "no warning should have been raised"
                Expect.isEmpty (Breakings result) "no breaking should have been raised"
            } 
            test "api definition with distinct infos/version is notified" {
                let previousApi = simpleInfoApi
                let currentApi = { simpleInfoApi with Infos = { 
                                                            simpleInfoApi.Infos with
                                                                Version = "1.0.1"
                                                            }
                                }
                let result = Compare previousApi currentApi
                Expect.hasLength (Infos result) 1 "changed infos should be an info"
                Expect.isEmpty (Warnings result) "no warning should have been raised"
                Expect.isEmpty (Breakings result) "no breaking should have been raised"
            }
            test "api definition with distinct infos/title is notified" {
                let previousApi = simpleInfoApi
                let currentApi = { simpleInfoApi with Infos = { 
                                                            simpleInfoApi.Infos with
                                                                Title = "new title"
                                                            }
                                }
                let result = Compare previousApi currentApi
                Expect.hasLength (Infos result) 1 "changed infos should be an info"
                Expect.isEmpty (Warnings result) "no warning should have been raised"
                Expect.isEmpty (Breakings result) "no breaking should have been raised"
            }             
            test "api definition with distinct operation path is blocking change" {
                let previousApi = apiOnePath
                let currentApi = { apiOnePath with Paths = [ {
                                                                apiOnePath.Paths.[0] with
                                                                    Path = "/newpath"
                                                             }
                                                           ]
                                }
                let result = Compare previousApi currentApi
                Expect.isEmpty (Infos result) "no info should have been raised"
                Expect.isEmpty (Warnings result) "no warning should have been raised"
                Expect.hasLength (Breakings result) 1 "one breaking should be raised"
            }
            test "api definition with distinct operation verb is blocking change" {
                let previousApi = apiOnePath
                let currentApi = { apiOnePath with Paths = [ {
                                                                apiOnePath.Paths.[0] with
                                                                    Verb = "POST"
                                                             }
                                                           ]
                                }
                let result = Compare previousApi currentApi
                Expect.isEmpty (Infos result) "no info should have been raised"
                Expect.isEmpty (Warnings result) "no warning should have been raised"
                Expect.hasLength (Breakings result) 1 "one breaking should be raised"
            }
        ]