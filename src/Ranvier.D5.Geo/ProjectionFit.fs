namespace Ranvier.D5.Geo

open System
open Ranvier.D5

/// <summary>Chooses projection scale and translation from the bounds of fully clipped, resampled geometry.</summary>
/// <remarks>Follows d3-geo fitting: geometry is projected at scale 150 with no viewport, its screen bounds are measured, and the scale and translation are solved so those bounds fill the target. Kind, rotation, center and clip angle are kept. Point positions count toward the bounds; symbol radii do not.</remarks>
module ProjectionFit =
    let private measure (options:GeoPathOptions) (config:ProjectionConfig) geometry =
        Projection.create {config with Scale=150.;Translate=Point.create 0. 0.}
        |> Result.bind(fun projection ->
            // A vanishing radius keeps each visible point's position without its symbol extent.
            GeoPath.project {options with ClipExtent=None;PointRadius=(if options.PointRadius>0. then 1e-9 else 0.)} projection geometry)
        |> Result.bind(fun paths ->
            let mutable left,top,right,bottom=infinity,infinity,-infinity,-infinity
            let extend (point:Point) =
                left<-min left point.X; right<-max right point.X
                top<-min top point.Y; bottom<-max bottom point.Y
            for path in paths do
                let mutable current=Point.create 0. 0.
                for command in Path.commands path do
                    match command with
                    | MoveTo point | LineTo point -> extend point; current<-point
                    | QuadraticTo(control,point) -> extend control; extend point; current<-point
                    | CubicTo(first,second,point) -> extend first; extend second; extend point; current<-point
                    | ArcTo(radiusX,radiusY,_,_,_,point) ->
                        // Geographic paths only emit complete circles as two half-arcs; bound each by its radius about the chord midpoint.
                        let radius=max radiusX radiusY
                        let middle=Point.create ((current.X+point.X)/2.) ((current.Y+point.Y)/2.)
                        extend (Point.create (middle.X-radius) (middle.Y-radius))
                        extend (Point.create (middle.X+radius) (middle.Y+radius))
                        extend point
                        current<-point
                    | Close -> ()
            if left>right || top>bottom then Error [InvalidObservation "Fitting requires geometry with a visible projected extent."]
            elif right-left<1e-6 && bottom-top<1e-6 then Error [InvalidObservation "Fitting requires geometry whose projected extent is not a single point."]
            else Ok(struct(left,top,right,bottom)))

    let private solve config scale x y =
        if not(Double.IsFinite scale && scale>0. && Double.IsFinite x && Double.IsFinite y) then
            Error [InvalidObservation "Fitted geometry has a degenerate projected extent."]
        else Projection.create {config with Scale=150.*scale;Translate=Point.create x y}

    /// <summary>Fits the projected geometry inside a rectangle, centred, preserving aspect ratio.</summary>
    let extent options (bounds:Bounds) geometry (config:ProjectionConfig) : Result<Projection,LayoutError list> =
        let width,height=Bounds.width bounds,Bounds.height bounds
        if not(width>0. && height>0.) then Error [InvalidObservation "Fitting requires a positive extent."]
        else
            measure options config geometry |> Result.bind(fun struct(left,top,right,bottom) ->
                let k=min (width/(right-left)) (height/(bottom-top))
                solve config k (Bounds.x bounds+(width-k*(right+left))/2.) (Bounds.y bounds+(height-k*(bottom+top))/2.))

    /// <summary>Fits the projected geometry inside a rectangle at the origin.</summary>
    let size options width height geometry config =
        if not(Double.IsFinite width && Double.IsFinite height && width>0. && height>0.) then Error [InvalidObservation "Fitting requires a positive size."]
        else extent options (Bounds.create 0. 0. width height) geometry config

    /// <summary>Fits the projected width, placing the top of the geometry at zero.</summary>
    let width options width geometry (config:ProjectionConfig) : Result<Projection,LayoutError list> =
        if not(Double.IsFinite width && width>0.) then Error [InvalidObservation "Fitting requires a positive width."]
        else
            measure options config geometry |> Result.bind(fun struct(left,top,right,_) ->
                let k=width/(right-left)
                solve config k ((width-k*(right+left))/2.) (-k*top))

    /// <summary>Fits the projected height, placing the left of the geometry at zero.</summary>
    let height options height geometry (config:ProjectionConfig) : Result<Projection,LayoutError list> =
        if not(Double.IsFinite height && height>0.) then Error [InvalidObservation "Fitting requires a positive height."]
        else
            measure options config geometry |> Result.bind(fun struct(left,top,_,bottom) ->
                let k=height/(bottom-top)
                solve config k (-k*left) ((height-k*(bottom+top))/2.))
