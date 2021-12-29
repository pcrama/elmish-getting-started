module Parameter

type VD<'t> = { Default : 't; Value : 't }

type ValueAndDefault =
    | S of VD<string>
    | I of VD<int32>
    | B of VD<bool>

type ValuesAndDefaults =
    | Ss of VD<string array>
    | Is of VD<int32 array>
    | Bs of VD<bool array>

let replaceValueInClone (src: VD<'t array>) (idx: int) (elt: 't): VD<'t array> =
    let newArray = Array.copy(src.Value)
    Array.set newArray idx elt
    { src with Value = newArray }

type ParameterLinkInformation = { Language: string option; Name: string; Message: string }

type INamedParameter =
    abstract member Name: string
    abstract member UiName: string

type NamedParameter(name: string, uiName: string) =
    interface INamedParameter with
        member _.Name = name
        member _.UiName = uiName

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
        member _.Name = name
        member _.UiName = uiName
        member _.ValueAndDefault = valueAndDefault
        member _.ValidationRules = validationRules
        member this.linkify(m) =
            { Language = None; Name = (this :> INamedParameter).Name; Message = m }
    member _.Description = description
    new(p: INamedParameter,
        valueAndDefault: ValueAndDefault,
        description: string,
        validationRules: ValidationRule list) =
        IndependentParameter(p.Name, p.UiName, valueAndDefault, description, validationRules)
    new(p: IValidatableParameter,
        description: string,
        validationRules: ValidationRule list) =
        IndependentParameter(p.Name, p.UiName, p.ValueAndDefault, description, validationRules)
    new(p: IndependentParameter, vad: ValueAndDefault) =
        let validatable = p :> IValidatableParameter
        match (validatable.ValueAndDefault, vad) with
        | (S _, S { Value = newValue }) -> IndependentParameter(p, newValue)
        | (I _, I { Value = newValue }) -> IndependentParameter(p, newValue)
        | (B _, B { Value = newValue }) -> IndependentParameter(p, newValue)
        | _ ->
            System.ArgumentException("Type error: trying to replace with incompatible value")
            |> raise
            // Make the compiler happy:
            IndependentParameter(
                "What is the compiler thinking?",
                "It should see that an error is raised so that this code",
                S { Default = "will"; Value = "never" },
                "be executed.",
                [])
    new(p: IndependentParameter, newValue: string) =
        let validatable = p :> IValidatableParameter
        let newValueAndDefault =
            match validatable.ValueAndDefault with
            | S oldVad ->
                S { oldVad with Value = newValue }
            | _ ->
                System.ArgumentException("Type error: trying to put string into incompatible parameter")
                |> raise
        IndependentParameter(
            validatable.Name,
            validatable.UiName,
            newValueAndDefault,
            p.Description,
            validatable.ValidationRules)
    new(p: IndependentParameter, newValue: int32) =
        let validatable = p :> IValidatableParameter
        let newValueAndDefault =
            match validatable.ValueAndDefault with
            | I oldVad ->
                I { oldVad with Value = newValue }
            | _ ->
                System.ArgumentException("Type error: trying to put int into incompatible parameter")
                |> raise
        IndependentParameter(
            validatable.Name,
            validatable.UiName,
            newValueAndDefault,
            p.Description,
            validatable.ValidationRules)
    new(p: IndependentParameter, newValue: bool) =
        let validatable = p :> IValidatableParameter
        let newValueAndDefault =
            match validatable.ValueAndDefault with
            | B oldVad ->
                B { oldVad with Value = newValue }
            | _ ->
                System.ArgumentException("Type error: trying to put boolean into incompatible parameter")
                |> raise
        IndependentParameter(
            validatable.Name,
            validatable.UiName,
            newValueAndDefault,
            p.Description,
            validatable.ValidationRules)

and LanguageParameter(name: string,
                      uiName: string,
                      valuesAndDefaults: ValuesAndDefaults,
                      description: string,
                      validationRules: ValidationRule list) =
    interface INamedParameter with
        member _.Name = name
        member _.UiName = uiName
    member _.ValidationRules = validationRules
    member _.Description = description
    member _.ValuesAndDefaults = valuesAndDefaults
    new(p: INamedParameter,
        valuesAndDefaults: ValuesAndDefaults,
        description: string,
        validationRules: ValidationRule list) =
        LanguageParameter(p.Name, p.UiName, valuesAndDefaults, description, validationRules)
    new(p: LanguageParameter, idx: int, vad: ValueAndDefault) =
        let named = p :> INamedParameter
        let newValuesAndDefaults =
            match (p.ValuesAndDefaults, vad) with
            | (Ss oldVads, S newVad) ->
                replaceValueInClone oldVads idx newVad.Value |> Ss
            | (Is oldVads, I newVad) ->
                replaceValueInClone oldVads idx newVad.Value |> Is
            | (Bs oldVads, B newVad) ->
                replaceValueInClone oldVads idx newVad.Value |> Bs
            | _ ->
                System.ArgumentException("Type error: trying to replace with incompatible value")
                |> raise
        LanguageParameter(
            named.Name,
            named.UiName,
            newValuesAndDefaults,
            p.Description,
            p.ValidationRules)
    new(p: LanguageParameter, idx: int, newValue: string) =
        let named = p :> INamedParameter
        let newValuesAndDefaults =
            match p.ValuesAndDefaults with
            | Ss oldVads ->
                replaceValueInClone oldVads idx newValue |> Ss
            | _ ->
                System.ArgumentException("Type error: trying to put string into incompatible parameter")
                |> raise
        LanguageParameter(
            named.Name,
            named.UiName,
            newValuesAndDefaults,
            p.Description,
            p.ValidationRules)
    new(p: LanguageParameter, idx: int, newValue: int32) =
        let named = p :> INamedParameter
        let newValuesAndDefaults =
            match p.ValuesAndDefaults with
            | Is oldVads ->
                replaceValueInClone oldVads idx newValue |> Is
            | _ ->
                System.ArgumentException("Type error: trying to put int into incompatible parameter")
                |> raise
        LanguageParameter(
            named.Name,
            named.UiName,
            newValuesAndDefaults,
            p.Description,
            p.ValidationRules)
    new(p: LanguageParameter, idx: int, newValue: bool) =
        let named = p :> INamedParameter
        let newValuesAndDefaults =
            match p.ValuesAndDefaults with
            | Bs oldVads ->
                replaceValueInClone oldVads idx newValue |> Bs
            | _ ->
                System.ArgumentException("Type error: trying to put boolean into incompatible parameter")
                |> raise
        LanguageParameter(
            named.Name,
            named.UiName,
            newValuesAndDefaults,
            p.Description,
            p.ValidationRules)

and MinimalParameter(name: string, uiName: string, valueAndDefault: ValueAndDefault, validationRules: ValidationRule list, forLanguage: string option) =
    interface IValidatableParameter with
        member _.Name = name
        member _.UiName = uiName
        member _.ValueAndDefault = valueAndDefault
        member _.ValidationRules = validationRules
        member this.linkify(m) =
            { Language = forLanguage; Name = (this :> INamedParameter).Name; Message = m }
    new(p: IValidatableParameter) = MinimalParameter(p.Name, p.UiName, p.ValueAndDefault, p.ValidationRules, None)
    new(p: LanguageParameter, idx: int, language: string) =
        let valAndDeflt =
            match p.ValuesAndDefaults with
            | Ss { Default = defaults ; Value = values } -> S { Default = defaults.[idx]; Value = values.[idx] }
            | Is { Default = defaults ; Value = values } -> I { Default = defaults.[idx]; Value = values.[idx] }
            | Bs { Default = defaults ; Value = values } -> B { Default = defaults.[idx]; Value = values.[idx] }
        MinimalParameter((p :> INamedParameter).Name, (p :> INamedParameter).UiName, valAndDeflt, p.ValidationRules, Some language)

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

let replaceByCloneIfNameMatch<'n when 'n :> INamedParameter> (name: string) (clone: 'n -> 'n) =
    let f p =
        let named = p :> INamedParameter
        if named.Name = name
        then clone(p)
        else p
    List.map f
