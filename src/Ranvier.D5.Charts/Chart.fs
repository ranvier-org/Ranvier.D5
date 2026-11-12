namespace Ranvier.D5.Charts

[<RequireQualifiedAccess>]
module Chart =
    let bar spec data = Cartesian.bar spec data
    let line spec data = Cartesian.line spec data
    let scatter spec data = Cartesian.scatter spec data
    let heatmap spec data = Cartesian.heatmap spec data
    let histogram spec data = Distribution.histogram spec data
    let boxPlot spec data = Distribution.boxPlot spec data
    let errorBars spec data = Distribution.errorBars spec data
    let waterfall spec data = Distribution.waterfall spec data
    let bullet spec data = Distribution.bullet spec data
    let lollipop spec data = Distribution.lollipop spec data
    let ohlc spec data = Distribution.ohlc spec data
    let pie spec data = Radial.pie spec data
    let radialBar spec data = Radial.radialBar spec data
    let radar spec data = Radial.radar spec data
    let parallelCoordinates spec data = Radial.parallelCoordinates spec data
    let calendar spec data = Radial.calendar spec data
    /// <summary>Points joined in Order; missing coordinates break the trail.</summary>
    let connectedScatter spec data = AdvancedCharts.connectedScatter spec data
    /// <summary>Before/after slopes between two shared vertical axes.</summary>
    let slope spec data = AdvancedCharts.slope spec data
    /// <summary>Per-period ranks joined by bump curves; ties break by ordinal series ID.</summary>
    let bump spec data = AdvancedCharts.bump spec data
    /// <summary>One folded band row per series, negatives mirrored.</summary>
    let horizon spec data = AdvancedCharts.horizon spec data
    /// <summary>Non-overlapping circles dodged from a centre line in X order.</summary>
    let beeswarm spec data = AdvancedCharts.beeswarm spec data
    /// <summary>A network as matrix cells; unknown endpoints and duplicate IDs are typed errors.</summary>
    let adjacencyMatrix spec = AdvancedCharts.adjacencyMatrix spec
