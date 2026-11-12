namespace Ranvier.D5

/// <summary>Typed failures returned in the Error case of validating constructors, parsers and layouts; each case carries an identifier or a
/// human-readable message.</summary>
type LayoutError =
    /// <summary>An identifier that occurs more than once where IDs must be unique; carries the duplicated ID.</summary>
    | DuplicateId of string
    /// <summary>A reference to an ID that is not present, such as a link endpoint or a node to move; carries the missing ID.</summary>
    | MissingEndpoint of string
    /// <summary>Input structure that cannot be laid out, such as a cycle, a non-square matrix or unstitchable contour rings; carries a
    /// message.</summary>
    | InvalidTopology of string
    /// <summary>A value or setting outside its accepted range, such as a non-finite number, a negative weight or an empty ID; carries a
    /// message.</summary>
    | InvalidObservation of string
    /// <summary>Input size, output size or work exceeds a configured budget; carries a message.</summary>
    | BudgetExceeded of string

/// <summary>The measured width and height of a text label, in layout units.</summary>
type TextSize = { Width:float; Height:float }
