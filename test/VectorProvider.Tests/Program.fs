open Expecto

let allTests =
  testList "All tests" [
    ConstructionTests.allTests
    ArithmeticTests.allTests
  ]

[<EntryPoint>]
let main _ =
  runTestsWithCLIArgs [] [||] allTests
