module Ranvier.D5.Gallery.ChordGallery

open System
open Ranvier.D5

let scene phase =
    let matrix=[[1.;2.+sin phase;0.5];[4.;0.;3.];[0.5;1.;2.]]
    let colors=[|Paint.rgb 45 120 200;Paint.rgb 230 120 45;Paint.rgb 45 160 110|]
    let panel mode centerX =
        let layout=Chord.layout {ChordConfig.defaultValue with Mode=mode;PadAngle=0.08} matrix |> Result.defaultWith(fun errors -> failwithf "%A" errors)
        let prefix=string mode
        let transform=Transform.translate centerX 290.
        [for flow in layout.Flows ->
            let spec={RibbonSpec.create 135. flow with ArrowHead=(if mode=ChordMode.Directed then Some 10. else None);PadAngle=0.012}
            Ribbon.path spec |> Mark.path(prefix+"/flow/"+string flow.Id)
            |> Mark.withFill colors[flow.Source.Index] |> Mark.withOpacity 0.55 |> Mark.withTransform transform]
        @[for group in layout.Groups ->
            Shape.arc 142. 154. group.StartAngle group.EndAngle |> Mark.path(prefix+"/group/"+string group.Index)
            |> Mark.withFill colors[group.Index] |> Mark.withTransform transform]
        @[for group in layout.Groups ->
            let angle=(group.StartAngle+group.EndAngle)/2.
            Mark.text (prefix+"/label/"+string group.Index) (Point.create (centerX+173.*sin angle) (290.-173.*cos angle)) (sprintf "%c: %.1f" (char(int 'A'+group.Index)) group.Value)
            |> Mark.withFontSize 15.
            |> fun mark -> Mark.withTextStyle {Mark.textStyle mark with Anchor="middle"} mark]
    Scene.create 800. 570. "Chord and directed ribbon foundations"
        ([Mark.text "title-left" (Point.create 70. 40.) "Bidirectional chord" |> Mark.withFontSize 23.
          Mark.text "title-right" (Point.create 470. 40.) "Directed arrow ribbon" |> Mark.withFontSize 23.]
         @panel ChordMode.Undirected 200.
         @panel ChordMode.Directed 600.
         @[Mark.text "help" (Point.create 35. 540.) "Pure matrix → angular layout → paths; a Ranvier signal changes the input." |> Mark.withFontSize 15.])
