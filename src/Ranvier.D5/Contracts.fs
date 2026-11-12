namespace Ranvier.D5

type LayoutError =
    | DuplicateId of string
    | MissingEndpoint of string
    | InvalidTopology of string
    | InvalidObservation of string
    | BudgetExceeded of string

type TextSize = { Width:float; Height:float }
