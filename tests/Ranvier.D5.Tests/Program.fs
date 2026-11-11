module Ranvier.D5.Tests.Program

open Expecto

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args DataScaleTests.tests
