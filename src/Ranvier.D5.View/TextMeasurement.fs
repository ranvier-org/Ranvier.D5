namespace Ranvier.D5.View

open System
open System.Collections.Generic
open System.Globalization
open Ranvier.D5

type TextMeasureRequest = { Text:string; FontFamily:string; FontWeight:int; FontSize:float; DeviceScale:float; FontGeneration:int64 }

/// <summary>An owner-thread cache with explicit font-generation and DPI identity. Dispose with the mounted chart owner.</summary>
type TextMeasureCache(capacity:int, measure:TextMeasureRequest -> TextSize) =
    let cache=Dictionary<TextMeasureRequest,TextSize>()
    let order=Queue<TextMeasureRequest>()
    let thread=Environment.CurrentManagedThreadId
    let mutable disposed=false
    do if capacity<1 then invalidArg "capacity" "Positive cache capacity required."
    member _.Count=cache.Count
    member _.Measure request =
        if disposed then raise(ObjectDisposedException(nameof TextMeasureCache))
        if Environment.CurrentManagedThreadId<>thread then invalidOp "Text measurement belongs to the owner thread."
        if isNull request.Text || String.IsNullOrWhiteSpace request.FontFamily then invalidArg "request" "Text and font family are required."
        if not(Double.IsFinite request.FontSize && Double.IsFinite request.DeviceScale) || request.FontSize<=0. || request.DeviceScale<=0. then invalidArg "request" "Positive finite size and scale required."
        match cache.TryGetValue request with
        | true,size -> size
        | _ ->
            let size=measure request
            if not(Double.IsFinite size.Width && Double.IsFinite size.Height) || size.Width<0. || size.Height<0. then invalidOp "Native text measurement returned invalid bounds."
            if cache.Count=capacity then cache.Remove(order.Dequeue()) |> ignore
            cache.Add(request,size)
            order.Enqueue request
            size
    interface IDisposable with
        member _.Dispose() = disposed<-true; cache.Clear(); order.Clear()

[<RequireQualifiedAccess>]
module TextMeasurement =
    /// <summary>Measures in SVG/CSS units using an existing host canvas. DeviceScale is a cache identity, not a second size multiplier.</summary>
    let native capacity (context:Ranvier.Dom.CanvasRenderingContext2D) =
        new TextMeasureCache(capacity, fun request ->
            let previous=context.font
            try
                context.font <- string request.FontWeight+" "+request.FontSize.ToString("G17",CultureInfo.InvariantCulture)+"px "+request.FontFamily
                let metrics=context.measureText request.Text
                {Width=max metrics.width (metrics.actualBoundingBoxLeft+metrics.actualBoundingBoxRight)
                 Height=max request.FontSize (metrics.actualBoundingBoxAscent+metrics.actualBoundingBoxDescent)}
            finally context.font<-previous)
