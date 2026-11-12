namespace Ranvier.D5.Charts

[<RequireQualifiedAccess>]
module Chart =
    let bar spec data = Cartesian.bar spec data
    let line spec data = Cartesian.line spec data
    let scatter spec data = Cartesian.scatter spec data
    let heatmap spec data = Cartesian.heatmap spec data
