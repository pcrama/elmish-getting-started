module Parameter

type VD<'t> = { Default : 't; Value : 't }

type ValueAndDefault =
    | S of VD<string>
    | I of VD<int>
    | B of VD<bool>
    
type Parameter =
    { Name : string
      UiName : string
      Description : string
      PerLanguage : bool
      ValidationRules : ValidationRule list
      ValueAndDefault : ValueAndDefault }

and ValidationRule = Parameter list -> string

let tryGetString (p : Parameter) : Result<string, string> =
    match p with
        | { ValueAndDefault = S { Value = v } } -> Ok v
        | { UiName = uiName } -> sprintf "Type error: '%s' is not a string." uiName |> Error

let tryGetInt32 (p : Parameter) : Result<int32, string> =
    match p with
        | { ValueAndDefault = I { Value = v } } -> Ok v
        | { UiName = uiName } -> sprintf "Type error: '%s' is not a number." uiName |> Error

let tryGetBool (p : Parameter) : Result<bool, string> =
    match p with
        | { ValueAndDefault = B { Value = v } } -> Ok v
        | { UiName = uiName } -> sprintf "Type error: '%s' is not a boolean." uiName |> Error

let rec extractValue tryGet name = function
    | [] -> sprintf "'%s' not found." name |> Error
    | p::_ when p.Name = name -> tryGet p
    | _::tail -> extractValue tryGet name tail

let extractString = extractValue tryGetString

let extractInt32 = extractValue tryGetInt32

let extractBool = extractValue tryGetBool

let applyOnOK defaultValue f = function
    | Ok v -> f v
    | Error _ -> defaultValue
