module Parameter

type VD<'t> = { Default : 't; Value : 't }

type ValueAndDefault =
    | S of VD<string>
    | I of VD<int>
    | B of VD<bool>

type ValuesAndDefaults =
    | Ss of VD<string array>
    | Is of VD<int array>
    | Bs of VD<bool array>

type INamedParameter =
    abstract member Name: string
    abstract member UiName: string

type NamedParameter(name: string, uiName: string) =
    interface INamedParameter with
        member this.Name = name
        member this.UiName = uiName

type ParameterLinkInformation = { Language: string option; Name: string; Message: string }

type IValidatableParameter =
    inherit INamedParameter
    abstract member ValueAndDefault: ValueAndDefault
    abstract member ValidationRules: ValidationRule list
    abstract member linkify: string -> ParameterLinkInformation

and IndependentParameter(name: string,
                         uiName: string,
                         valueAndDefault: ValueAndDefault,
                         description: string,
                         validationRules: ValidationRule list) =
    interface IValidatableParameter with
        member this.Name = name
        member this.UiName = uiName
        member this.ValueAndDefault = valueAndDefault
        member this.ValidationRules = validationRules
        member this.linkify(m) =
            { Language = None; Name = (this :> IValidatableParameter).Name; Message = m }
    member this.Description = description
    new(p: INamedParameter,
        valueAndDefault: ValueAndDefault,
        description: string,
        validationRules: ValidationRule list) =
        IndependentParameter(p.Name, p.UiName, valueAndDefault, description, validationRules)
    new(p: IValidatableParameter,
        description: string,
        validationRules: ValidationRule list) =
        IndependentParameter(p.Name, p.UiName, p.ValueAndDefault, description, validationRules)

and LanguageParameter(name: string,
                      uiName: string,
                      valuesAndDefaults: ValuesAndDefaults,
                      description: string,
                      validationRules: ValidationRule list) =
    interface INamedParameter with
        member this.Name = name
        member this.UiName = uiName
    member this.ValidationRules = validationRules
    member this.Description = description
    member this.ValuesAndDefaults = valuesAndDefaults
    new(p: INamedParameter,
        valuesAndDefaults: ValuesAndDefaults,
        description: string,
        validationRules: ValidationRule list) =
        LanguageParameter(p.Name, p.UiName, valuesAndDefaults, description, validationRules)

and MinimalParameter(name: string, uiName: string, valueAndDefault: ValueAndDefault, forLanguage: string option) =
    interface IValidatableParameter with
        member this.Name = name
        member this.UiName = uiName
        member this.ValueAndDefault = valueAndDefault
        member this.ValidationRules = []
        member this.linkify(m) =
            { Language = forLanguage; Name = (this :> IValidatableParameter).Name; Message = m }
    new(p: IValidatableParameter) = MinimalParameter(p.Name, p.UiName, p.ValueAndDefault, None)
    new(p: LanguageParameter, idx: int, language: string) =
        let valAndDeflt =
            match p.ValuesAndDefaults with
            | Ss { Default = defaults ; Value = values } -> S { Default = defaults.[idx]; Value = values.[idx] }
            | Is { Default = defaults ; Value = values } -> I { Default = defaults.[idx]; Value = values.[idx] }
            | Bs { Default = defaults ; Value = values } -> B { Default = defaults.[idx]; Value = values.[idx] }
        MinimalParameter((p :> INamedParameter).Name, (p :> INamedParameter).UiName, valAndDeflt, Some language)

and ValidationRule = IValidatableParameter list -> string

let tryGetString (p: IValidatableParameter): Result<string, string> =
    match p.ValueAndDefault with
    | S { Value = v } -> Ok v
    | _ -> sprintf "Type error: '%s' is not a string." p.UiName |> Error

let tryGetInt32 (p: IValidatableParameter): Result<int32, string> =
    match p.ValueAndDefault with
    | I { Value = v } -> Ok v
    | _ -> sprintf "Type error: '%s' is not a number." p.UiName |> Error

let tryGetBool (p : IValidatableParameter) : Result<bool, string> =
    match p.ValueAndDefault with
    | B { Value = v } -> Ok v
    | _ -> sprintf "Type error: '%s' is not a boolean." p.UiName |> Error

let rec extractValue tryGet name (ps: IValidatableParameter list) =
    match List.tryFind (fun (p: IValidatableParameter) -> p.Name = name) ps with
    | None -> sprintf "'%s' not found." name |> Error
    | Some p -> tryGet p

let extractString = extractValue tryGetString

let extractInt32 = extractValue tryGetInt32

let extractBool = extractValue tryGetBool

let applyOnOK defaultValue f = function
    | Ok v -> f v
    | Error _ -> defaultValue
