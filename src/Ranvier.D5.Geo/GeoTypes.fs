namespace Ranvier.D5.Geo

/// <summary>Immutable JSON values. Number payloads preserve the JSON numeric lexeme.</summary>
[<RequireQualifiedAccess>]
type GeoValue =
    | Null
    | Boolean of bool
    | Number of string
    | String of string
    | Array of GeoValue list
    | Object of Map<string,GeoValue>

/// <summary>A feature identifier with distinct string and exact JSON-number representations.</summary>
[<RequireQualifiedAccess>]
type GeoId = Text of string | Number of string

/// <summary>Unit-sphere geometry with degree positions and shortest great-circle edges.</summary>
/// <remarks>Small exterior polygon rings are clockwise; holes have opposite winding.
/// Reversing a ring can select its spherical complement. Point None and empty collections represent empty geometry.</remarks>
[<RequireQualifiedAccess>]
type GeoGeometry =
    | Point of GeoPosition option
    | MultiPoint of GeoPosition list
    | LineString of GeoPosition list
    | MultiLineString of GeoPosition list list
    | Polygon of GeoPosition list list
    | MultiPolygon of GeoPosition list list list
    | Collection of GeoGeometry list
    /// <summary>The complete unit sphere; this extension has no RFC 7946 serialization.</summary>
    | Sphere

/// <summary>A feature with optional identity and geometry, nullable properties, and preserved foreign members.</summary>
type GeoFeature =
    { Id: GeoId option
      Geometry: GeoGeometry option
      Properties: Map<string,GeoValue> option
      ForeignMembers: Map<string,GeoValue> }

/// <summary>An RFC 7946 document interpreted as spherical geometry.</summary>
[<RequireQualifiedAccess>]
type GeoDocument =
    | Geometry of GeoGeometry
    | Feature of GeoFeature
    | FeatureCollection of GeoFeature list * Map<string,GeoValue>

/// <summary>Finite JSON size, nesting, geometry and traversal budgets.</summary>
type GeoJsonConfig =
    { MaxJsonBytes: int
      MaxDepth: int
      MaxPoints: int
      MaxFeatures: int
      MaxWork: int64 }

/// <summary>Default geographic interchange budgets.</summary>
module GeoJsonConfig =
    /// <summary>Allows 32 MB UTF-8 JSON, depth 64, one million points, 100,000 features and ten million visits.</summary>
    let defaultValue =
        { MaxJsonBytes=32000000; MaxDepth=64; MaxPoints=1000000; MaxFeatures=100000; MaxWork=10000000L }
