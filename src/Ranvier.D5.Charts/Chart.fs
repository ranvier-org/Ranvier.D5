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
