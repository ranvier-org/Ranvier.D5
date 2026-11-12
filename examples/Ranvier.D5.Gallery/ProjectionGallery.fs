module Ranvier.D5.Gallery.ProjectionGallery

open Ranvier.D5
open Ranvier.D5.Geo

let private ok = function Ok value -> value | Error errors -> failwithf "%A" errors
let private p longitude latitude=GeoPosition.create longitude latitude |> ok

let private rgb (r,g,b)=Paint.rgb r g b
let oceanColor,landColor,planarColor,backgroundColor=(222,234,243),(74,135,102),(214,120,64),(255,255,255)
let private ocean,land,planarFill=rgb oceanColor,rgb landColor,rgb planarColor
let private ink=Paint.rgb 31 44 65
let private grid=Paint.rgb 170 186 201
let private route=Paint.rgb 205 70 52

let equirectangularPanel=Bounds.create 20. 95. 370. 200.
let mercatorPanel=Bounds.create 410. 95. 370. 200.
let orthographicPanel=Bounds.create 20. 345. 370. 200.
let planarPanel=Bounds.create 410. 345. 370. 200.
let planarClip=Bounds.create 430. 365. 330. 160.

let private center bounds=Point.create (Bounds.x bounds+Bounds.width bounds/2.) (Bounds.y bounds+Bounds.height bounds/2.)
let private geographic kind scale bounds longitude latitude =
    Projection.create {ProjectionConfig.defaultValue kind with
                        Scale=scale;Translate=center bounds
                        Rotation=GeoRotation.create longitude latitude 0. |> ok} |> ok

/// <summary>Projections for a phase in [0,1]: the seam split and the globe limb both keep crossing the polygon while the polar meridian moves.</summary>
let projections phase =
    geographic ProjectionKind.Equirectangular (Bounds.width equirectangularPanel/(2.*System.Math.PI)) equirectangularPanel (-10.+15.*phase) 0.,
    geographic ProjectionKind.Mercator (Bounds.height mercatorPanel/(2.*System.Math.PI)) mercatorPanel (-20.*phase) 0.,
    geographic ProjectionKind.Orthographic 92. orthographicPanel (-100.-20.*phase) -20.,
    IdentityProjection.create {IdentityProjectionConfig.defaultValue with
                                Scale=1.6;Translate=center planarPanel;ReflectY=true;AngleDegrees=15.*phase} |> ok

/// <summary>A clockwise seam-crossing exterior with a counterclockwise hole, in the D3 spherical ring convention.</summary>
let seamPolygon=
    GeoGeometry.Polygon [
        [p 140. -35.;p 140. 35.;p -140. 35.;p -140. -35.;p 140. -35.]
        [p 160. -12.;p -160. -12.;p -160. 12.;p 160. 12.;p 160. -12.]]
let private polarCap=GeoGeometry.Polygon [[for longitude in 180. .. -30. .. -150. -> p longitude 72.]@[p 180. 72.]]
let private polarMeridian=GeoGeometry.LineString [p 60. -89.;p 60. 89.]
let private crossing=GeoGeometry.LineString [p 120. 48.;p -100. 18.]
let private cities=GeoGeometry.MultiPoint [p -122.4 37.8;p 151.2 -33.9;p -0.1 51.5;p 139.7 35.7]
let private graticule=GeoGeometry.MultiLineString (Graticule.create GraticuleConfig.defaultValue |> ok).Lines
/// <summary>A planar exterior larger than the clip, with an enclosed hole of opposite winding.</summary>
let planarPolygon=
    PlanarGeometry.Polygon [
        [Point.create -110. -55.;Point.create 110. -55.;Point.create 110. 55.;Point.create -110. 55.;Point.create -110. -55.]
        [Point.create -30. -18.;Point.create -30. 18.;Point.create 30. 18.;Point.create 30. -18.;Point.create -30. -18.]]
let private planarLine=PlanarGeometry.LineString [for i in 0..40 -> Point.create (float i*6.-120.) (35.*sin(float i/4.))]

let private marks key style (paths:Path list) = paths |> List.mapi(fun index path -> Mark.path (key+"/"+string index) path |> style)
let private filled color mark=mark |> Mark.withFill color
let private stroked color width mark=mark |> Mark.withFill Paint.none |> Mark.withStroke color width
let private label key (bounds:Bounds) text=
    Mark.text key (Point.create (Bounds.x bounds) (Bounds.y bounds-9.)) text |> Mark.withFontSize 14. |> Mark.withFill ink
let private frame key (bounds:Bounds) = Shape.rectangle bounds |> Mark.path key |> stroked grid 1.

let private world prefix projection (bounds:Bounds) features =
    let options={GeoPathOptions.defaultValue with ClipExtent=Some bounds;PointRadius=3.5}
    let draw geometry=GeoPath.project options projection geometry |> ok
    [yield! draw GeoGeometry.Sphere |> marks (prefix+"/sphere") (filled ocean)
     yield! draw graticule |> marks (prefix+"/graticule") (stroked grid 0.75)
     for name,geometry,style in features do yield! draw geometry |> marks (prefix+"/"+name) style]

/// <summary>Draws the four bounded projection panels for a phase in [0,1].</summary>
let scene phase =
    let equirectangular,mercator,orthographic,identity=projections phase
    let planarOptions={GeoPathOptions.defaultValue with ClipExtent=Some planarClip;PointRadius=3.}
    let planar geometry=PlanarPath.project planarOptions identity geometry |> ok
    let shared=["land",seamPolygon,filled land;"route",crossing,stroked route 2.;"cities",cities,filled ink]
    Scene.create 800. 570. "Bounded geographic projections"
        ([Mark.text "title" (Point.create 20. 34.) "Bounded geographic projections" |> Mark.withFontSize 22. |> Mark.withFill ink
          Mark.text "subtitle" (Point.create 20. 58.) (sprintf "One Ranvier signal drives all four panels (phase %.2f). Holes stay holes; output stays inside each clip." phase) |> Mark.withFontSize 13. |> Mark.withFill ink
          label "equirectangular-label" equirectangularPanel "Equirectangular: polygon and hole split at the antimeridian"
          label "mercator-label" mercatorPanel "Mercator: polar cap clipped at ±85.05° before projection"
          label "orthographic-label" orthographicPanel "Orthographic: hidden hemisphere clipped at the limb"
          label "planar-label" planarPanel "Planar identity: reflected, rotated, viewport-clipped"]
         @world "equirectangular" equirectangular equirectangularPanel shared
         @world "mercator" mercator mercatorPanel ["cap",polarCap,filled land;"meridian",polarMeridian,stroked route 2.]
         @world "orthographic" orthographic orthographicPanel shared
         @(planar planarPolygon |> marks "planar/polygon" (filled planarFill))
         @(planar planarLine |> marks "planar/line" (stroked ink 1.5))
         @[frame "equirectangular/frame" equirectangularPanel;frame "mercator/frame" mercatorPanel
           frame "orthographic/frame" orthographicPanel;frame "planar/frame" planarPanel;frame "planar/clip" planarClip])

/// <summary>Screen samples whose painted color proves fill, hole and clipping behavior at a phase.</summary>
let probes phase =
    let equirectangular,mercator,orthographic,identity=projections phase
    let at projection longitude latitude=Projection.project projection (p longitude latitude) |> Option.get
    ["equirectangular seam land",at equirectangular -145. 24.,landColor
     "equirectangular seam hole",at equirectangular 165. 5.,oceanColor
     "mercator polar cap",at mercator 15. 78.,landColor
     "mercator below cap",at mercator 15. 64.,oceanColor
     "orthographic visible land",at orthographic 145. 25.,landColor
     "orthographic visible hole",at orthographic 165. 5.,oceanColor
     "planar ring",IdentityProjection.project identity (Point.create -80. 0.) |> Option.get,planarColor
     "planar hole",IdentityProjection.project identity (Point.create 5. 3.) |> Option.get,backgroundColor]
