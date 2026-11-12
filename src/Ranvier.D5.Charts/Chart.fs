namespace Ranvier.D5.Charts

/// <summary>Pure chart recipes returning a ChartOutput or typed LayoutError values; an invalid frame, empty or duplicate keys, and invalid observations are errors, not exceptions.</summary>
[<RequireQualifiedAccess>]
module Chart =
    /// <summary>One rectangle per datum with a value, semantic ID from Key; non-finite values and duplicate category/series cells are typed errors.</summary>
    let bar spec data = Cartesian.bar spec data
    /// <summary>One path per series on linear scales with a 5 px hit point per complete observation; missing coordinates break the path.</summary>
    let line spec data = Cartesian.line spec data
    /// <summary>One circle per datum with both coordinates; a negative or non-finite area is a typed error.</summary>
    let scatter spec data = Cartesian.scatter spec data
    /// <summary>One band cell per datum with a value; duplicate column/row cells and non-finite values are typed errors.</summary>
    let heatmap spec data = Cartesian.heatmap spec data
    /// <summary>One bar per bin over a zero-based count axis; each bin's semantic and hit IDs are its members' keys.</summary>
    let histogram spec data = Distribution.histogram spec data
    /// <summary>Quartile boxes, whiskers and outlier points per datum; non-finite samples and a negative Tukey multiplier are typed errors.</summary>
    let boxPlot spec data = Distribution.boxPlot spec data
    /// <summary>One vertical interval with caps and an estimate tick per datum; intervals not ordered Lower &lt;= Estimate &lt;= Upper are typed errors.</summary>
    let errorBars spec data = Distribution.errorBars spec data
    /// <summary>Running-total steps in input order, colored by direction; totals draw from zero.</summary>
    let waterfall spec data = Distribution.waterfall spec data
    /// <summary>Range bands, a measure bar and a target marker per datum; invalid or unordered ranges are typed errors.</summary>
    let bullet spec data = Distribution.bullet spec data
    /// <summary>A stem from zero and a circular head per datum with a value.</summary>
    let lollipop spec data = Distribution.lollipop spec data
    /// <summary>OHLC ticks or candlesticks per datum; non-finite prices or a Low/High range not enclosing Open and Close are typed errors.</summary>
    let ohlc spec data = Distribution.ohlc spec data
    /// <summary>Pie or donut slices in input order clockwise from 12 o'clock with sector hit targets; zero slices get no label.</summary>
    let pie spec data = Radial.pie spec data
    /// <summary>Equal-angle radial bars scaled to the largest value; negative or non-finite values and out-of-range ratios are typed errors.</summary>
    let radialBar spec data = Radial.radialBar spec data
    /// <summary>One radar outline per datum, filled only when every dimension has a value.</summary>
    let radar spec data = Radial.radar spec data
    /// <summary>One polyline per datum across evenly spaced vertical axes; missing values break the line.</summary>
    let parallelCoordinates spec data = Radial.parallelCoordinates spec data
    /// <summary>One square per dated value; duplicate dates, non-finite values and an invalid week start are typed errors.</summary>
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
