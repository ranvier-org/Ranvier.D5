module Ranvier.D5.Tests.Program

open Expecto

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args (testList "D5" [DataScaleTests.tests; GeometryTests.tests; ReactiveTests.tests; FixtureTests.tests; SceneCompositionTests.tests; ChartLayoutTests.tests; StatisticsTests.tests; ScaleTimeTests.tests; CurveStackTests.tests; ChartRecipeTests.tests; DistributionTests.tests; CompositionTests.tests; InteractionTests.tests; ChromaticTests.tests; KeyedTransitionTests.tests; ReactiveLayoutTests.tests; HierarchyBuildTests.tests; HierarchyLayoutTests.tests; TreemapTests.tests; PackTests.tests; HierarchyChartTests.tests; PlanarTests.tests; QuadtreeTests.tests; DelaunayTests.tests; VoronoiTests.tests])
