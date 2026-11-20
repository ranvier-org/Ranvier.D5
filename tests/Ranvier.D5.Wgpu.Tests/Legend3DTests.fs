module Ranvier.D5.Wgpu.Tests.Legend3DTests

open System
open Expecto
open Ranvier.D5
open Ranvier.D5.Wgpu
open Ranvier.Wgpu

[<Tests>]
let legend3DTests =
    testSequenced
    <| testList
        "Ranvier.D5.Wgpu.Legend3D"
        [
            testCase "Legend3D.colorbar emits 33-stop LinearGradient, fill rect, border, nice ticks and title for both vertical and horizontal specs"
            <| fun () ->
                let ramp = ColorRamp.viridis (0.0, 100.0)
                let vBounds = Bounds.create 720.0 80.0 18.0 240.0
                let vSpec = ColorbarSpec3D.vertical vBounds "Temperature (K)" (0.0, 100.0) ramp
                Expect.equal vSpec.Orientation LegendOrientation3D.Vertical "ColorbarSpec3D.vertical sets Vertical"

                let vScene = Legend3D.colorbar 800.0 600.0 vSpec
                Expect.isTrue (Scene.isComposed vScene) "colorbar emits composed Scene"

                let defs = Scene.definitions vScene
                Expect.equal defs.Length 1 "colorbar emits 1 LinearGradient definition"

                match defs.Head with
                | SceneDefinition.LinearGradient (key, startPt, endPt, stops) ->
                    Expect.equal stops.Length 33 "LinearGradient has 33 stops"
                    Expect.floatClose { absolute = 1e-9; relative = 0.0 } stops.Head.Offset 0.0 "first stop at offset 0"
                    Expect.floatClose { absolute = 1e-9; relative = 0.0 } (List.last stops).Offset 1.0 "last stop at offset 1"
                    Expect.isGreaterThan startPt.Y endPt.Y "vertical colorbar gradient runs bottom-to-top (min at bottom, max at top)"
                    let barMark = Scene.marks vScene |> List.find (fun m -> Mark.fillDefinition m = Some key)
                    Expect.equal (Mark.fillDefinition barMark) (Some key) "gradient bar references gradient definition key"
                | other -> failtestf "Expected LinearGradient, got %A" other

                let marks = Scene.marks vScene
                Expect.isTrue (marks |> List.exists (fun m -> Mark.key m = "legend3d/colorbar/title")) "colorbar emits title mark"
                Expect.isTrue (marks |> List.exists (fun m -> Mark.key m = "legend3d/colorbar/border")) "colorbar emits border mark"
                Expect.isTrue (marks |> List.exists (fun m -> (Mark.key m).StartsWith "legend3d/colorbar/label/")) "colorbar emits tick labels"

                let hBounds = Bounds.create 200.0 540.0 300.0 16.0
                let hSpec =
                    { ColorbarSpec3D.horizontal hBounds "Pressure" (-5.0, 5.0) (ColorRamp.coolWarm (-5.0, 5.0)) with
                        FormatTick = Some (fun v -> $"{v:F1} bar") }
                Expect.equal hSpec.Orientation LegendOrientation3D.Horizontal "ColorbarSpec3D.horizontal sets Horizontal"
                let hScene = Legend3D.colorbar 800.0 600.0 hSpec
                match (Scene.definitions hScene).Head with
                | SceneDefinition.LinearGradient (_, startPt, endPt, _) ->
                    Expect.isLessThan startPt.X endPt.X "horizontal colorbar gradient runs left-to-right"
                | _ -> failtest "Expected horizontal LinearGradient"
                Expect.isTrue (Scene.marks hScene |> List.exists (fun m -> match Mark.geometry m with TextGeometry (_, t) -> t.EndsWith " bar" | _ -> false)) "custom FormatTick applied"

                Expect.throwsT<ArgumentOutOfRangeException> (fun () -> Legend3D.colorbar 0.0 600.0 vSpec |> ignore) "colorbar rejects non-positive viewportWidth"

            testCase "Legend3D.categorical emits swatch boxes and labels with dim visibility and Legend3D.combine merges scenes uniquely"
            <| fun () ->
                let items : LegendSwatch3D list =
                    [
                        { Id = "core"; Label = "Core Module"; Color = RgbaColor (0.2f, 0.7f, 0.9f, 1.0f); Visible = true }
                        { Id = "test"; Label = "Tests (hidden)"; Color = RgbaColor (0.9f, 0.4f, 0.2f, 1.0f); Visible = false }
                    ]

                let catScene = Legend3D.categorical 800.0 600.0 (Point.create 24.0 32.0) (Some "Clusters") items
                Expect.isTrue (Scene.isComposed catScene) "categorical returns composed Scene"
                let catMarks = Scene.marks catScene
                Expect.isTrue (catMarks |> List.exists (fun m -> match Mark.geometry m with TextGeometry (_, t) -> t = "Clusters" | _ -> false)) "categorical title emitted"
                let coreBox = catMarks |> List.find (fun m -> Mark.key m = "legend3d/swatch/core/box")
                let hiddenBox = catMarks |> List.find (fun m -> Mark.key m = "legend3d/swatch/test/box")
                Expect.floatClose { absolute = 1e-6; relative = 0.0 } (Mark.style coreBox).Opacity 1.0 "visible swatch opacity 1.0"
                Expect.isLessThan (Mark.style hiddenBox).Opacity 0.5 "hidden swatch dimmed opacity"

                let cbSpec = ColorbarSpec3D.vertical (Bounds.create 720.0 80.0 18.0 240.0) "Metric" (0.0, 1.0) (ColorRamp.plasma (0.0, 1.0))
                let cbScene1 = Legend3D.colorbar 800.0 600.0 cbSpec
                let cbScene2 = Legend3D.colorbar 800.0 600.0 (ColorbarSpec3D.horizontal (Bounds.create 200.0 540.0 240.0 16.0) "Metric 2" (-1.0, 1.0) (ColorRamp.cividis (-1.0, 1.0)))
                let hudScene = Scene.create 800.0 600.0 "HUD" [ Mark.text "hud/status" (Point.create 20.0 580.0) "Ready" ]

                let combined = Legend3D.combine 800.0 600.0 [ cbScene1; catScene; hudScene; cbScene2 ]
                Expect.isTrue (Scene.isComposed combined) "combine returns composed Scene"
                Expect.equal (Scene.definitions combined).Length 2 "combine preserves distinct gradient definitions from both colorbars"
                let combinedMarks = Scene.marks combined
                Expect.isTrue (combinedMarks |> List.exists (fun m -> Mark.key m = "hud/status")) "combine preserves HUD text mark"
                Expect.isTrue (combinedMarks |> List.exists (fun m -> Mark.key m = "legend3d/swatch/core/box")) "combine preserves categorical swatches"
        ]
