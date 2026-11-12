module Ranvier.D5.Tests.Program

open Expecto

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args (testList "D5" [DataScaleTests.tests; GeometryTests.tests; ReactiveTests.tests; FixtureTests.tests; SceneCompositionTests.tests; ChartLayoutTests.tests; StatisticsTests.tests; ScaleTimeTests.tests])
