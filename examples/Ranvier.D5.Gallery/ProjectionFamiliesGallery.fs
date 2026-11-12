module Ranvier.D5.Gallery.ProjectionFamiliesGallery

open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private p longitude latitude=GeoPosition.create longitude latitude |> ok
let private rgb (r,g,b)=Paint.rgb r g b
let oceanColor,landColor,stateColor=(222,234,243),(74,135,102),(196,112,62)
let private ink=Paint.rgb 31 44 65
let private grid=Paint.rgb 170 186 201
let private route=Paint.rgb 205 70 52

let private panel column row=Bounds.create (15.+float column*262.) (80.+float row*158.) 250. 140.
let private families=[
    "Azimuthal equal-area",ProjectionKind.AzimuthalEqualArea
    "Azimuthal equidistant",ProjectionKind.AzimuthalEquidistant
    "Gnomonic (60° cap)",ProjectionKind.Gnomonic
    "Stereographic (142° cap)",ProjectionKind.Stereographic
    "Lambert conformal conic",ProjectionKind.ConicConformal(20.,50.)
    "Albers equal-area conic",ProjectionKind.ConicEqualArea(29.5,45.5)
    "Equidistant conic",ProjectionKind.ConicEquidistant(20.,60.)
    "Transverse Mercator",ProjectionKind.TransverseMercator]

let private ellipse longitude latitude rx ry clockwise =
    let ring=[for index in 0..12 -> let angle=(if clockwise then -1. else 1.)*float index*System.Math.PI/6. in p (longitude+rx*cos angle) (latitude+ry*sin angle)]
    ring@[List.head ring]
/// <summary>A clockwise exterior with a counterclockwise hole; probes avoid the ten-degree graticule.</summary>
let land=GeoGeometry.Polygon [ellipse 15. 20. 70. 40. true;ellipse 15. 20. 28. 16. false]
let private crossing=GeoGeometry.LineString [p -60. 10.;p 80. 50.]
let private graticule=GeoGeometry.MultiLineString (Graticule.create GraticuleConfig.defaultValue |> ok).Lines
// The conformal pole is clamped near infinity, so that panel fits a latitude band instead of the whole Sphere.
let private conformalRegion=GeoGeometry.MultiPoint [for longitude in -180. .. 30. .. 180. do for latitude in [-50.;0.;80.] -> p longitude latitude]
let private states=GeoGeometry.MultiPolygon [
    for longitude,latitude,width,height in [-109.,37.,7.,4.;-104.,41.,7.,4.;-97.,33.,6.,5.;-90.,38.,6.,4.;-160.,60.,14.,6.;-156.,18.9,1.5,1.3] ->
        [[p longitude latitude;p longitude (latitude+height);p (longitude+width) (latitude+height);p (longitude+width) latitude;p longitude latitude]]]

let private inset (bounds:Bounds) = Bounds.create (Bounds.x bounds+4.) (Bounds.y bounds+4.) (Bounds.width bounds-8.) (Bounds.height bounds-8.)

/// <summary>Each family fitted to its panel for a phase in [0,1]; the phase rotates longitude.</summary>
let projections phase =
    families |> List.mapi(fun index (name,kind) ->
        let bounds=panel (index%3) (index/3)
        let config={ProjectionConfig.defaultValue kind with Rotation=GeoRotation.create (-10.+30.*phase) 0. 0. |> ok}
        let options={GeoPathOptions.defaultValue with PointRadius=1.}
        let fitted=ProjectionFit.extent options (inset bounds) (if index=4 then conformalRegion else GeoGeometry.Sphere) config |> ok
        name,bounds,fitted)

let usaPanel=panel 2 2
let usa=CompositeProjection.albersUsa (1070.*Bounds.width usaPanel/960.) (Point.create (Bounds.x usaPanel+Bounds.width usaPanel/2.) (Bounds.y usaPanel+Bounds.height usaPanel/2.)) |> ok

let private marks key style (paths:Path list) = paths |> List.mapi(fun index path -> Mark.path (key+"/"+string index) path |> style)
let private filled color mark=mark |> Mark.withFill color
let private stroked color width mark=mark |> Mark.withFill Paint.none |> Mark.withStroke color width
let private label key (bounds:Bounds) text=Mark.text key (Point.create (Bounds.x bounds) (Bounds.y bounds-7.)) text |> Mark.withFontSize 13. |> Mark.withFill ink
let private frame key (bounds:Bounds) = Shape.rectangle bounds |> Mark.path key |> stroked grid 1.

/// <summary>Draws every projection family and the Albers USA composite for a phase in [0,1].</summary>
let scene phase =
    let family index (name,bounds,projection) =
        let prefix=sprintf "family%d" index
        let options={GeoPathOptions.defaultValue with ClipExtent=Some bounds;Precision=0.25}
        let draw geometry=GeoPath.project options projection geometry |> ok
        [yield label (prefix+"/label") bounds name
         yield! draw GeoGeometry.Sphere |> marks (prefix+"/sphere") (filled(rgb oceanColor))
         yield! draw graticule |> marks (prefix+"/graticule") (stroked grid 0.6)
         yield! draw land |> marks (prefix+"/land") (filled(rgb landColor))
         yield! draw crossing |> marks (prefix+"/route") (stroked route 1.5)
         yield frame (prefix+"/frame") bounds]
    let states=CompositeProjection.paths {GeoPathOptions.defaultValue with Precision=0.25} usa states |> ok
    let insets=CompositeProjection.parts usa |> List.mapi(fun index part -> frame (sprintf "usa/inset%d" index) part.Clip)
    Scene.create 800. 570. "Projection families"
        ([Mark.text "title" (Point.create 15. 32.) "Projection families, fitted to their panels" |> Mark.withFontSize 22. |> Mark.withFill ink
          Mark.text "subtitle" (Point.create 15. 54.) (sprintf "One Ranvier signal rotates every family (phase %.2f). Each panel is sized by ProjectionFit from clipped geometry." phase) |> Mark.withFontSize 13. |> Mark.withFill ink]
         @(projections phase |> List.mapi family |> List.concat)
         @[label "usa/label" usaPanel "Albers USA composite insets"]
         @(states |> marks "usa/states" (filled(rgb stateColor)))
         @insets
         @[frame "usa/frame" usaPanel])

/// <summary>Screen samples whose painted color proves land fill, open holes and inset placement at a phase.</summary>
let probes phase =
    [for name,_,projection in projections phase do
        let at longitude latitude=Projection.project projection (p longitude latitude) |> Option.get
        yield name+" land",at 15. 45.,ProjectionGallery.Matches landColor
        yield name+" hole",at 15. 25.,ProjectionGallery.Differs landColor
     for name,longitude,latitude in ["Colorado inset",-105.5,39.;"Alaska inset",-153.,63.;"Hawaii inset",-155.2,19.6] ->
        name,CompositeProjection.project usa (p longitude latitude) |> Option.get,ProjectionGallery.Matches stateColor]
