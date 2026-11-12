namespace Ranvier.D5.Geo

open System.Threading
open Ranvier.D5

/// <summary>Geometry events in source order. Polygon rings omit their duplicate closing vertex.</summary>
[<Struct; RequireQualifiedAccess>]
type GeoStreamEvent =
    | Point of GeoPosition
    | LineStart
    | LineEnd
    | PolygonStart
    | PolygonEnd
    | Sphere

/// <summary>Limits for input vertices, feature/geometry-collection nesting and total traversal work.</summary>
type GeoStreamConfig = { MaxPoints: int; MaxDepth: int; MaxWork: int64 }

/// <summary>Default limits for bounded spherical traversal.</summary>
module GeoStreamConfig =
    /// <summary>Allows one million input vertices, depth 64 and ten million work units.</summary>
    let defaultValue = {MaxPoints=1_000_000;MaxDepth=64;MaxWork=10_000_000L}

/// <summary>Functional folds over geographic geometry without materializing an event array.</summary>
module GeoStream =
    exception private Rejected of LayoutError
    /// <summary>Folds a document, propagating cancellation and returning validation or budget failures.</summary>
    /// <remarks>Work counts containers, input vertices and emitted events. Closing ring vertices count toward MaxPoints.
    /// Use a pure folder: malformed later input can fail after earlier callbacks. Folder exceptions propagate.
    /// Coordinates and winding are preserved; arc ambiguity is handled by downstream spherical algorithms.</remarks>
    let foldDocumentWithCancellation (config: GeoStreamConfig) (token: CancellationToken) (folder: 'State -> GeoStreamEvent -> 'State) (state: 'State) (document: GeoDocument) : Result<'State,LayoutError list> =
        token.ThrowIfCancellationRequested()
        if config.MaxPoints < 1 || config.MaxDepth < 1 || config.MaxDepth > 128 || config.MaxWork < 1L then
            Error [InvalidObservation "Geographic traversal requires positive budgets and depth in [1,128]."]
        else
            try
                let mutable current=state
                let mutable points=0
                let mutable work=0L
                let visit () =
                    token.ThrowIfCancellationRequested()
                    if work >= config.MaxWork then raise(Rejected(BudgetExceeded "Geographic traversal exceeds its work budget."))
                    work <- work+1L
                let container depth =
                    visit()
                    if depth > config.MaxDepth then raise(Rejected(BudgetExceeded "Geographic traversal exceeds its depth budget."))
                let emit event =
                    visit()
                    current <- folder current event
                let inputPoint () =
                    visit()
                    if points >= config.MaxPoints then raise(Rejected(BudgetExceeded "Geographic traversal exceeds its point budget."))
                    points <- points+1
                let point position =
                    inputPoint()
                    emit(GeoStreamEvent.Point position)
                let line ring (positions: GeoPosition list) =
                    visit()
                    emit GeoStreamEvent.LineStart
                    let mutable count=0
                    let first=match positions with head::_ -> ValueSome head | [] -> ValueNone
                    let mutable remaining=positions
                    while not(List.isEmpty remaining) do
                        let position=remaining.Head
                        remaining <- remaining.Tail
                        inputPoint()
                        count <- count+1
                        if ring && List.isEmpty remaining then
                            match first with
                            | ValueSome start when start.Longitude=position.Longitude && start.Latitude=position.Latitude -> ()
                            | _ -> raise(Rejected(InvalidObservation "Polygon rings must close at their first coordinate."))
                        else emit(GeoStreamEvent.Point position)
                    if (ring && count < 4) || (not ring && count=1) then
                        raise(Rejected(InvalidObservation "Lines require zero or at least two coordinates; polygon rings require at least four closed coordinates."))
                    emit GeoStreamEvent.LineEnd
                let polygon (rings: GeoPosition list list) =
                    visit()
                    emit GeoStreamEvent.PolygonStart
                    for ring in rings do line true ring
                    emit GeoStreamEvent.PolygonEnd
                let rec geometry depth value =
                    container depth
                    match value with
                    | GeoGeometry.Point position -> Option.iter point position
                    | GeoGeometry.MultiPoint positions -> List.iter point positions
                    | GeoGeometry.LineString positions -> line false positions
                    | GeoGeometry.MultiLineString lines -> List.iter (line false) lines
                    | GeoGeometry.Polygon rings -> polygon rings
                    | GeoGeometry.MultiPolygon polygons -> List.iter polygon polygons
                    | GeoGeometry.Collection children -> for child in children do geometry (depth+1) child
                    | GeoGeometry.Sphere -> emit GeoStreamEvent.Sphere
                let feature depth (value: GeoFeature) =
                    container depth
                    value.Geometry |> Option.iter (geometry (depth+1))
                match document with
                | GeoDocument.Geometry value -> geometry 1 value
                | GeoDocument.Feature value -> feature 1 value
                | GeoDocument.FeatureCollection(features,_) ->
                    container 1
                    for value in features do feature 2 value
                token.ThrowIfCancellationRequested()
                Ok current
            with Rejected error -> Error [error]

    /// <summary>Folds a document without cancellation.</summary>
    let foldDocument config folder state document = foldDocumentWithCancellation config CancellationToken.None folder state document

    /// <summary>Folds a geometry with cancellation.</summary>
    let foldWithCancellation config token folder state geometry = foldDocumentWithCancellation config token folder state (GeoDocument.Geometry geometry)

    /// <summary>Folds a geometry without cancellation.</summary>
    let fold config folder state geometry = foldWithCancellation config CancellationToken.None folder state geometry
