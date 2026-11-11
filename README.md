# Ranvier.D5

Functional-first visualization foundations for native F# applications. Compose immutable data, scales, geometry and scenes; let Ranvier signals update retained SVG through View and native WebCore. No JavaScript, HTML or CSS files are required.

## Packages

- **Ranvier.D5**: data summaries, bins, keyed joins, numeric/categorical/UTC scales, axes, affine transforms, paths, lines/areas/arcs/symbols, pie/stack/tree/treemap layouts, spatial queries, reactive values, animation, zoom and brush.
- **Ranvier.D5.View**: keyed SVG scene components and native mark events.
- Both have a **.Traced** variant. Use one variant throughout the dependency graph.

Target: .NET 10. Version: **0.1.0-alpha.1**. APIs may change during alpha. [Contracts and limits](docs/contracts.md) describe the algorithms and integration responsibilities.

## A reactive scene

```fsharp
open Ranvier
open Ranvier.D5
open Ranvier.D5.View

// Inside the host's Ranvier graph/owner:
let values = Api.createSignal [10.; 30.; 20.]
let scene =
    Reactive.derive (fun () -> values.Value) (fun data ->
        let x = Scale.linear (0., 2.) (20., 280.)
        let y = Scale.linear (0., 40.) (180., 20.)
        let points =
            data |> List.mapi (fun i v ->
                Some(Point.create (Scale.map x (float i)) (Scale.map y v)))
        let line =
            Shape.line Curve.Linear points
            |> Mark.path "series"
            |> Mark.withFill Paint.none
            |> Mark.withStroke (Paint.rgb 45 120 210) 2.
        Scene.create 300. 200. "Measurements" [line])

// Mount Renderer.passive with (fun () -> scene.Value) through View.
// Writing values.Value updates the retained series mark.
```

Use `Renderer.create (fun key eventName event -> ...)` for mark events. Use `Ranvier.Anime.Ease.named "inOutCubic"` with `Transition.start` or `Reactive.animate`; the latter consumes your clock signal and creates no timer. `Paint.ofCss` accepts existing typed Ranvier.Css values.

The [native gallery](examples/Ranvier.D5.Gallery) includes the full mount, graph and window lifetime:

```sh
dotnet run --project examples/Ranvier.D5.Gallery -c Release
dotnet run --project examples/Ranvier.D5.Gallery -c Release -- --capture gallery.bmp
```

It renders a signal-driven line, donut, treemap and bars using the same scene primitives.

## Build and verify

```sh
dotnet run --project tests/Ranvier.D5.Tests -c Release
dotnet run --project tests/Ranvier.D5.Native.Tests -c Release
dotnet run --project benchmarks/Ranvier.D5.Benchmarks -c Release
dotnet pack src/Ranvier.D5 -c Release -o artifacts/packages
dotnet pack src/Ranvier.D5.View -c Release -o artifacts/packages
```

Append `-p:RanvierTrace=true` for Traced. Native acceptance requires WebCore's platform libraries; [CI](.github/workflows/ci.yml) lists Ubuntu dependencies. Windows, Linux and macOS run normal and Traced tests, native rendering, performance gates, gallery capture and packing.

## Kit snapshot and compatibility

[kit.lock.json](kit.lock.json) records eight committed source revisions: Ranvier, Anime, CSS, View, WebCore, Persistence, Examples and Docs. Existing local edits were excluded and preserved. Recreate detached source checkouts in a new directory:

```sh
python tools/snapshot-kit.py ../snapshots
```

Snapshots are reference material; builds use a compatible published cohort: Ranvier preview.2, Anime rc.1, CSS alpha.2, View 0.1.49-rc.2 and WebCore backend rc.6 / native rc.7. Newer independent core/CSS packages are deliberately not substituted because Anime/View pin this cohort. See [design](docs/design.md).

D5 provides reusable foundations, not every D3 algorithm. Geographic clipping/projections, contours, force simulation and specialized graph layouts remain extensions. Large scenes need aggregation/windowing; SVG does not promise million-mark frame rates.

MIT licensed.
