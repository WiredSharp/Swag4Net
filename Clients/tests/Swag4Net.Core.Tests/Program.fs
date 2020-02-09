namespace Swag4Net.Core.Tests

module Program =
  open Expecto
  
  (**
   * run the test with:
   * dotnet watch -p Swag4Net.Core.Tests.fsproj run
   *)
  
  let [<EntryPoint>] main args =

    let runTests = runTestsWithArgs defaultConfig args

    let result =
      [ Swag4Net.Core.Tests.v2.ParsingTests.tests
        //Swag4Net.Core.Tests.v3.ParsingTests.tests
        Swag4Net.Core.Tests.ApiDiffTests.tests
        Swag4Net.Core.Tests.ParserTests.tests
        Swag4Net.Core.Tests.ValidatorTests.tests ]
      |> List.fold (fun r t -> r + runTests t) 0
    
    result
