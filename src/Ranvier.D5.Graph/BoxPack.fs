namespace Ranvier.D5.Graph

open System
open System.Collections.Generic
open Ranvier.D5

/// <summary>Settings for Skyline/Shelf aspect-ratio rectangle packing (<c>elk.box</c> / component packing).</summary>
type BoxPackConfig =
    { /// <summary>Target viewport bounds into which the packed diagram is positioned.</summary>
      Bounds: Bounds
      /// <summary>Desired width-to-height aspect ratio (positive finite; default 1.5).</summary>
      TargetAspectRatio: float
      /// <summary>Minimum spacing gap between sibling boxes in layout units.</summary>
      Padding: float }

/// <summary>Constructors for <c>BoxPackConfig</c>.</summary>
[<RequireQualifiedAccess>]
module BoxPackConfig =
    /// <summary>Creates a rectangle-packing configuration with 1.5 aspect ratio and 16-unit padding.</summary>
    let create bounds = { Bounds = bounds; TargetAspectRatio = 1.5; Padding = 16. }

/// <summary>Skyline / Shelf aspect-ratio rectangle packer for compound groups and disconnected graph components.</summary>
[<RequireQualifiedAccess>]
module BoxPack =
    /// <summary>Packs rectangles <c>(id, width, height)</c> into non-overlapping <c>Bounds</c> near origin <c>(originX, originY)</c>.</summary>
    let internal packRaw
        (originX: float)
        (originY: float)
        (targetAspect: float)
        (padding: float)
        (items: (string * float * float)[])
        : Bounds[] =
        let n = items.Length
        if n = 0 then [||]
        elif n = 1 then
            let _, w, h = items[0]
            [| Bounds.create originX originY w h |]
        else
            let pad = max 0. padding
            let aspect = if Double.IsFinite targetAspect && targetAspect > 0.05 then targetAspect else 1.5
            let order = Array.init n id
            Array.Sort(order, Comparer<int>.Create(fun a b ->
                let idA, wA, hA = items[a]
                let idB, wB, hB = items[b]
                let c = compare hB hA
                if c <> 0 then c
                else
                    let c2 = compare wB wA
                    if c2 <> 0 then c2 else StringComparer.Ordinal.Compare(idA, idB)))

            let mutable totalArea = 0.
            let mutable maxW = 0.
            let mutable sumW = 0.
            for i = 0 to n - 1 do
                let _, w, h = items[i]
                totalArea <- totalArea + (w + pad) * (h + pad)
                if w > maxW then maxW <- w
                sumW <- sumW + w + pad

            let idealWidth = max maxW (sqrt (totalArea * aspect))
            let candidates =
                [| max maxW (idealWidth * 0.7)
                   max maxW (idealWidth * 0.85)
                   idealWidth
                   min sumW (idealWidth * 1.18)
                   min sumW (idealWidth * 1.4) |]

            let evaluate stripLimit =
                let placed = Array.zeroCreate<Bounds> n
                // Skyline nodes: (x, width, y)
                let sky = ResizeArray<struct(float * float * float)>()
                sky.Add(struct(0., max stripLimit sumW, 0.))
                let mutable usedW = 0.
                let mutable usedH = 0.

                for rank = 0 to n - 1 do
                    let idx = order[rank]
                    let _, w, h = items[idx]
                    let reqW = w + (if rank < n - 1 then pad else 0.)

                    let findSlot limitW =
                        let mutable bestSkyIdx = -1
                        let mutable bestY = infinity
                        let mutable bestX = infinity
                        for s = 0 to sky.Count - 1 do
                            let struct(sx, _, _) = sky[s]
                            if sx + w <= limitW + 1e-9 || s = 0 then
                                // Compute max Y across spanned skyline segments
                                let mutable rem = reqW
                                let mutable k = s
                                let mutable maxY = 0.
                                let mutable fits = sx + w <= limitW + 1e-9
                                while rem > 1e-9 && k < sky.Count do
                                    let struct(_, kw, ky) = sky[k]
                                    if ky > maxY then maxY <- ky
                                    rem <- rem - kw
                                    k <- k + 1
                                if rem > 1e-9 then fits <- false
                                if fits && (maxY < bestY - 1e-9 || (abs (maxY - bestY) <= 1e-9 && sx < bestX)) then
                                    bestY <- maxY
                                    bestX <- sx
                                    bestSkyIdx <- s
                        bestSkyIdx, bestX, bestY

                    let mutable sIdx, bx, by = findSlot stripLimit
                    if sIdx < 0 then
                        let s2, x2, y2 = findSlot (max stripLimit sumW)
                        sIdx <- max 0 s2
                        bx <- if Double.IsFinite x2 then x2 else 0.
                        by <- if Double.IsFinite y2 then y2 else usedH + pad

                    placed[idx] <- Bounds.create (originX + bx) (originY + by) w h
                    if bx + w > usedW then usedW <- bx + w
                    if by + h > usedH then usedH <- by + h

                    // Insert new skyline segment [bx, bx + reqW] at height (by + h + pad)
                    let newTop = by + h + pad
                    let endX = bx + reqW
                    let updated = ResizeArray<struct(float * float * float)>(sky.Count + 2)
                    let mutable inserted = false
                    for s = 0 to sky.Count - 1 do
                        let struct(sx, sw, sy) = sky[s]
                        let sEnd = sx + sw
                        if sEnd <= bx + 1e-9 then
                            updated.Add(sky[s])
                        elif sx >= endX - 1e-9 then
                            if not inserted then
                                updated.Add(struct(bx, reqW, newTop))
                                inserted <- true
                            updated.Add(sky[s])
                        else
                            if sx < bx - 1e-9 then
                                updated.Add(struct(sx, bx - sx, sy))
                            if not inserted then
                                updated.Add(struct(bx, reqW, newTop))
                                inserted <- true
                            if sEnd > endX + 1e-9 then
                                updated.Add(struct(endX, sEnd - endX, sy))
                    if not inserted then
                        updated.Add(struct(bx, reqW, newTop))

                    // Merge adjacent skyline segments with identical height
                    sky.Clear()
                    for seg in updated do
                        if sky.Count = 0 then sky.Add seg
                        else
                            let struct(px, pw, py) = sky[sky.Count - 1]
                            let struct(cx, cw, cy) = seg
                            if abs (py - cy) <= 1e-9 && abs (px + pw - cx) <= 1e-9 then
                                sky[sky.Count - 1] <- struct(px, pw + cw, py)
                            else
                                sky.Add seg

                let actualAspect = if usedH <= 1e-9 then aspect else usedW / usedH
                let aspectPenalty = 1. + 0.35 * abs (log (max 0.05 actualAspect / aspect))
                let score = max 1. (usedW * usedH) * aspectPenalty
                score, placed

            let mutable bestScore = infinity
            let mutable bestPlaced = [||]
            for limit in candidates do
                let sc, pl = evaluate limit
                if sc < bestScore then
                    bestScore <- sc
                    bestPlaced <- pl
            bestPlaced

    /// <summary>Arranges multiple sub-layouts (e.g. disconnected components) into a single packed <c>DiagramLayout2D</c>.</summary>
    let internal packComponentLayouts (bounds: Bounds) (padding: float) (components: DiagramLayout2D[]) : DiagramLayout2D =
        if components.Length = 0 then
            { Bounds = bounds; Nodes = []; Groups = []; Associations = [] }
        elif components.Length = 1 then
            let single = components[0]
            let dx = Bounds.x bounds - Bounds.x single.Bounds
            let dy = Bounds.y bounds - Bounds.y single.Bounds
            GraphKernels.translateLayout dx dy single
        else
            let targetAspect =
                if Bounds.height bounds > 1e-6 && Bounds.width bounds > 1e-6 then
                    Bounds.width bounds / Bounds.height bounds
                else 1.5
            let rawItems =
                components
                |> Array.mapi (fun i c ->
                    let id = match c.Nodes with h :: _ -> h.Id | [] -> string i
                    id, max 1. (Bounds.width c.Bounds), max 1. (Bounds.height c.Bounds))
            let packedBoxes = packRaw (Bounds.x bounds) (Bounds.y bounds) targetAspect padding rawItems
            let shifted =
                components
                |> Array.mapi (fun i c ->
                    let target = packedBoxes[i]
                    GraphKernels.translateLayout (Bounds.x target - Bounds.x c.Bounds) (Bounds.y target - Bounds.y c.Bounds) c)
            let nodes = shifted |> Seq.collect (fun c -> c.Nodes) |> Seq.sortWith (fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id)) |> Seq.toList
            let groups =
                shifted
                |> Seq.collect (fun c -> c.Groups)
                |> Seq.sortWith (fun a b ->
                    let c = compare a.Depth b.Depth
                    if c <> 0 then c else StringComparer.Ordinal.Compare(a.Id, b.Id))
                |> Seq.toList
            let assocs = shifted |> Seq.collect (fun c -> c.Associations) |> Seq.sortWith (fun a b -> StringComparer.Ordinal.Compare(a.Id, b.Id)) |> Seq.toList
            { Bounds = GraphKernels.computeDiagramBounds bounds nodes groups assocs
              Nodes = nodes
              Groups = groups
              Associations = assocs }

    /// <summary>Packs compound groups recursively bottom-up and positions member nodes cleanly inside their enclosing containers (<c>elk.box</c>).</summary>
    let layoutWithCancellation
        (token: System.Threading.CancellationToken)
        (config: BoxPackConfig)
        (graph: DiagramGraph)
        : Result<DiagramLayout2D, LayoutError list> =
        token.ThrowIfCancellationRequested()
        if not (Double.IsFinite config.TargetAspectRatio) || config.TargetAspectRatio <= 0.
           || not (Double.IsFinite config.Padding) || config.Padding < 0. then
            Error [ InvalidObservation "BoxPack requires positive finite TargetAspectRatio and nonnegative finite Padding." ]
        else
            let eff = DiagramGraph.effective graph
            let n = eff.Nodes.Length
            let gCount = eff.Groups.Length
            if n = 0 && gCount = 0 then
                Ok { Bounds = config.Bounds; Nodes = []; Groups = []; Associations = [] }
            else
                let gIdx = Dictionary<string, int>(StringComparer.Ordinal)
                for gi = 0 to gCount - 1 do gIdx.Add(eff.Groups[gi].Id, gi)

                let directNodes = Array.init gCount (fun _ -> ResizeArray<int>())
                let rootNodes = ResizeArray<int>()
                for ni = 0 to n - 1 do
                    match eff.Nodes[ni].ParentGroup with
                    | Some gid when gIdx.ContainsKey gid -> directNodes[gIdx[gid]].Add ni
                    | _ -> rootNodes.Add ni

                let childGroups = Array.init gCount (fun _ -> ResizeArray<int>())
                let rootGroups = ResizeArray<int>()
                for gi = 0 to gCount - 1 do
                    match eff.Groups[gi].ParentGroup with
                    | Some pid when gIdx.ContainsKey pid -> childGroups[gIdx[pid]].Add gi
                    | _ -> rootGroups.Add gi

                let nodeLocal = Array.zeroCreate<Bounds> n
                let groupOuter = Array.zeroCreate<Bounds> gCount
                let groupInner = Array.zeroCreate<Bounds> gCount

                let rec shiftSubtreeGroup (gi: int) (dx: float) (dy: float) =
                    let ob = groupOuter[gi]
                    let ib = groupInner[gi]
                    groupOuter[gi] <- Bounds.create (Bounds.x ob + dx) (Bounds.y ob + dy) (Bounds.width ob) (Bounds.height ob)
                    groupInner[gi] <- Bounds.create (Bounds.x ib + dx) (Bounds.y ib + dy) (Bounds.width ib) (Bounds.height ib)
                    for ni in directNodes[gi] do
                        let nb = nodeLocal[ni]
                        nodeLocal[ni] <- Bounds.create (Bounds.x nb + dx) (Bounds.y nb + dy) (Bounds.width nb) (Bounds.height nb)
                    for cgi in childGroups[gi] do
                        shiftSubtreeGroup cgi dx dy

                // Bottom-up packing of every compound group
                for gi = gCount - 1 downto 0 do
                    token.ThrowIfCancellationRequested()
                    let g = eff.Groups[gi]
                    let pad = g.Padding
                    let items = ResizeArray<Choice<int, int> * string * float * float>()
                    for ni in directNodes[gi] do
                        let nd = eff.Nodes[ni]
                        items.Add(Choice1Of2 ni, "n:" + nd.Id, nd.Size.Width, nd.Size.Height)
                    for cgi in childGroups[gi] do
                        let cb = groupOuter[cgi]
                        items.Add(Choice2Of2 cgi, "g:" + eff.Groups[cgi].Id, Bounds.width cb, Bounds.height cb)

                    if items.Count = 0 then
                        let w, h = g.MinSize.Width, g.MinSize.Height
                        groupOuter[gi] <- Bounds.create 0. 0. w h
                        groupInner[gi] <- Bounds.create pad.Left pad.Top (max 0. (w - pad.Left - pad.Right)) (max 0. (h - pad.Top - pad.Bottom))
                    else
                        let raw = items |> Seq.map (fun (_, k, w, h) -> k, w, h) |> Seq.toArray
                        let packed = packRaw pad.Left pad.Top config.TargetAspectRatio config.Padding raw
                        let mutable unionInner = packed[0]
                        for k = 0 to packed.Length - 1 do
                            let b = packed[k]
                            if k > 0 then unionInner <- Bounds.union unionInner b
                            let tag, _, _, _ = items[k]
                            match tag with
                            | Choice1Of2 ni -> nodeLocal[ni] <- b
                            | Choice2Of2 cgi ->
                                let old = groupOuter[cgi]
                                shiftSubtreeGroup cgi (Bounds.x b - Bounds.x old) (Bounds.y b - Bounds.y old)
                        groupInner[gi] <- unionInner
                        let rawW = Bounds.x unionInner - pad.Left + Bounds.width unionInner + pad.Left + pad.Right
                        let rawH = Bounds.y unionInner - pad.Top + Bounds.height unionInner + pad.Top + pad.Bottom
                        groupOuter[gi] <- Bounds.create 0. 0. (max g.MinSize.Width rawW) (max g.MinSize.Height rawH)

                // Top-level packing of root nodes and root groups inside config.Bounds
                let topItems = ResizeArray<Choice<int, int> * string * float * float>()
                for ni in rootNodes do
                    let nd = eff.Nodes[ni]
                    topItems.Add(Choice1Of2 ni, "n:" + nd.Id, nd.Size.Width, nd.Size.Height)
                for gi in rootGroups do
                    let gb = groupOuter[gi]
                    topItems.Add(Choice2Of2 gi, "g:" + eff.Groups[gi].Id, Bounds.width gb, Bounds.height gb)

                let topRaw = topItems |> Seq.map (fun (_, k, w, h) -> k, w, h) |> Seq.toArray
                let topPacked = packRaw (Bounds.x config.Bounds) (Bounds.y config.Bounds) config.TargetAspectRatio config.Padding topRaw
                for k = 0 to topPacked.Length - 1 do
                    let b = topPacked[k]
                    let tag, _, _, _ = topItems[k]
                    match tag with
                    | Choice1Of2 ni -> nodeLocal[ni] <- b
                    | Choice2Of2 gi ->
                        let old = groupOuter[gi]
                        shiftSubtreeGroup gi (Bounds.x b - Bounds.x old) (Bounds.y b - Bounds.y old)

                let posNodes =
                    eff.Nodes
                    |> Array.mapi (fun ni nd ->
                        let b =
                            match nd.Pinned with
                            | Some p -> Bounds.create (p.X - nd.Size.Width * 0.5) (p.Y - nd.Size.Height * 0.5) nd.Size.Width nd.Size.Height
                            | None -> nodeLocal[ni]
                        let cx = Bounds.x b + Bounds.width b * 0.5
                        let cy = Bounds.y b + Bounds.height b * 0.5
                        { Id = nd.Id
                          Kind = nd.Kind
                          Bounds = b
                          Center = Point.create cx cy
                          ParentGroup = nd.ParentGroup
                          Layer = None
                          Ports = GraphKernels.resolveNodePorts nd b Map.empty })

                let posGroups =
                    [ for gi = 0 to gCount - 1 do
                        let g = eff.Groups[gi]
                        yield
                            { Id = g.Id
                              ParentGroup = g.ParentGroup
                              Bounds = groupOuter[gi]
                              InnerBounds = groupInner[gi]
                              Collapsed = false
                              Depth = eff.GroupDepth[gi] } ]

                let routed = GraphKernels.routeDirectAssociations eff posNodes None
                let nodesList = Array.toList posNodes
                Ok
                    { Bounds = GraphKernels.computeDiagramBounds config.Bounds nodesList posGroups routed
                      Nodes = nodesList
                      Groups = posGroups
                      Associations = routed }

    /// <summary>Packs compound groups and nodes without cancellation.</summary>
    let layout config graph = layoutWithCancellation System.Threading.CancellationToken.None config graph
