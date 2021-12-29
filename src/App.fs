module App

open Elmish
open Elmish.React
open Feliz

// NB: there is also Feliz.Bulma, but I did not want an extra dependency.
// Fable.Core.JsInterop.importAll "App.css"

open Parameter

type Validated<'t> =
    { Raw : string
      Parsed : Option<'t> }

module Validated =
    let createEmpty() : Validated<_> =
        { Raw = ""; Parsed = None }

    let success raw value : Validated<_> =
        { Raw = raw; Parsed = Some value }

    let failure raw : Validated<_> =
        { Raw = raw; Parsed = None }

    let iter f v =
        match v.Parsed with
        | Some x -> f x |> Some
        | None -> None

let parseValidate parse s =
    try Validated.success s (parse s)
    with | _ -> Validated.failure s

type Configuration =
    { Languages: string list
      LanguageDependent: LanguageParameter list
      Parameters: IndependentParameter list
      Name: string }

type ActiveTab =
    | General
    | LanguageIndependent
    | LanguageDependent

type LinkedMessage = { Tab: ActiveTab; Message: string }

type State =
    { Configuration: Configuration
      ActiveTab: ActiveTab
      ValidationErrors: LinkedMessage list
    }

type Msg =
    | SwitchTab of ActiveTab
    | SetBool of string * string Option * bool
    | SetString of string * string Option * string
    | SetInt32 of string * string Option * int32
    | SetLanguageOrder of string list
    | SetProjectName of string

let findLanguage lang =
    List.findIndex (fun x -> x = lang)

let reshuffleLanguages
        (oldLangs: string list)
        (data: LanguageParameter list)
        (newLangs : string list)
        : LanguageParameter list =
    let indices = List.map (fun newLang -> findLanguage newLang oldLangs) newLangs
    let shuffle (xs: 'a array): 'a array =
        [| for idx in indices -> xs.[idx] |]
    let shuffleVD { Default = ds; Value = vs } = { Default = shuffle ds; Value = shuffle vs }
    let shuffleValuesAndDefaults = function
        | Ss ss -> shuffleVD ss |> Ss
        | Is is -> shuffleVD is |> Is
        | Bs bs -> shuffleVD bs |> Bs
    let shuffleParameter (p: LanguageParameter) =
        new LanguageParameter(
            p,
            shuffleValuesAndDefaults p.ValuesAndDefaults,
            p.Description,
            p.ValidationRules)
    List.map shuffleParameter data

let validateHasStringPrefix (prefix: string) (p: INamedParameter): ValidationRule =
    let rule (ps: IValidatableParameter list) =
        extractString p.Name ps
        |> applyOnOK (sprintf "Type error: '%s' is not a string." p.UiName)
                     (fun s -> if s.StartsWith(prefix)
                               then ""
                               else sprintf "'%s' should start with '%s'." p.UiName prefix)
    rule

let validateHasMinMaxLength (min: int) (max: int) (p: INamedParameter): ValidationRule =
    let rule (ps: IValidatableParameter list) =
        extractString p.Name ps
        |> applyOnOK (sprintf "Type error: '%s' is not a string." p.UiName)
                     (fun s -> let len = s.Length
                               if ((min <= len) && (len <= max))
                               then ""
                               else sprintf "'%s' should be between %d and %d characters." p.UiName min max)
    rule

let newConfiguration () =
    let mkLangParam t1 t2 = [(let p = NamedParameter("s1", "First string")
                              new LanguageParameter(
                                  p,
                                  Ss { Default = [| t1; t1 |]; Value = [| t1; t1 |] },
                                  "This first string has a validation rule concerning its length.",
                                  [validateHasMinMaxLength 1 10 p]))
                             (let p = NamedParameter("s2", "Second string")
                              new LanguageParameter(
                                  p,
                                  Ss { Default = [| t2; t2 |]; Value = [| t2; t2 |] },
                                  "This second string has a validation rule concerning its prefix.",
                                  [validateHasStringPrefix "s2" p]))]
    { Languages = ["English"; "French"]
      LanguageDependent = mkLangParam "t1" "t2"
      Parameters = [(let p = NamedParameter("s3", "Third string")
                     new IndependentParameter(
                        p,
                        S { Default = "s3 value"; Value = "s3 value" },
                        "The third string, which is language independent",
                        [validateHasStringPrefix "s3" p]))
                    new IndependentParameter(
                        "i1",
                        "First integer",
                        I { Default = 3; Value = 3 },
                        "The 1st int, which is language independent",
                        [])
                    new IndependentParameter(
                        "b1",
                        "First boolean",
                        B { Default = true; Value = true },
                        "The 1st boolean, which is language independent",
                        [])]
      Name = "Demo configuration" }

let replaceIndependentParameters (state: State) (name: string) (clone: IndependentParameter -> IndependentParameter) =
    let conf = state.Configuration
    { state with Configuration = { conf with Parameters = replaceByCloneIfNameMatch name clone conf.Parameters }}

let replaceLanguageParameters (state: State) (language: string) (name: string) (clone: int -> LanguageParameter -> LanguageParameter) =
    let idx = findLanguage language state.Configuration.Languages
    let conf = state.Configuration
    { state with Configuration = { conf with LanguageDependent = replaceByCloneIfNameMatch name (clone idx) conf.LanguageDependent }}

let updateNoValidate (msg: Msg) (state: State): State =
    match msg with
    | SwitchTab newTab -> { state with ActiveTab = newTab }
    | SetLanguageOrder newLangs ->
        let newDependents = reshuffleLanguages (state.Configuration.Languages)
                                               (state.Configuration.LanguageDependent)
                                               newLangs
        let newConfiguration = { state.Configuration with
                                     Languages = newLangs
                                     LanguageDependent = newDependents }
        { state with Configuration = newConfiguration }
    | SetProjectName newName -> { state with Configuration = { state.Configuration with Name = newName}}
    | SetString (name, None, newValue) ->
        let clone p = IndependentParameter(p, newValue)
        replaceIndependentParameters state name clone
    | SetString (name, Some language, newValue) ->
        let clone idx p = LanguageParameter(p, idx, newValue)
        replaceLanguageParameters state language name clone
    | SetInt32 (name, None, newValue) ->
        let clone p = IndependentParameter(p, newValue)
        replaceIndependentParameters state name clone
    | SetInt32 (name, Some language, newValue) ->
        let clone idx p = LanguageParameter(p, idx, newValue)
        replaceLanguageParameters state language name clone
    | SetBool (name, None, newValue) ->
        let clone p = IndependentParameter(p, newValue)
        replaceIndependentParameters state name clone
    | SetBool (name, Some language, newValue) ->
        let clone idx p = LanguageParameter(p, idx, newValue)
        replaceLanguageParameters state language name clone

let validateParameters (env: IValidatableParameter list) =
    List.map (fun (p: IValidatableParameter) -> List.map (fun r -> r env |> p.linkify) p.ValidationRules) env
    |> List.map (List.filter (fun { Message = s } -> not (s = null || s = "")))
    |> List.concat
    |> List.map (function
                 | { Language = Some _; Message = m } -> { Tab = LanguageDependent; Message = m }
                 | { Language = None; Message = m } -> { Tab = LanguageIndependent; Message = m })

let rec enumerateList i = function
    | [] -> []
    | x::tail -> (i, x)::enumerateList (i + 1) tail

let validateState state =
    let errorsInGeneralTab =
        if state.Configuration.Name.Trim() = ""
        then [{ Tab = General; Message = "Configuration Project Name may not be blank." }]
        else []
    let castToValidatable x = x :> IValidatableParameter
    let validatableLanguageIndependent = List.map castToValidatable state.Configuration.Parameters
    let validatableLanguageDependent idx languageName =
        state.Configuration.LanguageDependent
        |> List.map (fun p -> new MinimalParameter(p, idx, languageName) :> IValidatableParameter)
    let envs: (IValidatableParameter list) list =
        match state.Configuration.Languages with
        | [] -> [validatableLanguageIndependent]
        | languages ->
            enumerateList 0 languages
            |> List.map (fun (idx, languageName) ->
                             List.concat [validatableLanguageDependent idx languageName
                                          validatableLanguageIndependent])
    List.concat [errorsInGeneralTab
                 List.map validateParameters envs |> List.concat]

let update (msg: Msg) (state: State): State =
    let newState = updateNoValidate msg state
    { newState with ValidationErrors = validateState newState }

let renderTabHeader (activeTab: ActiveTab) (dispatch: Msg -> unit) =
    let className wanted = if wanted = activeTab then "is-active" else ""
    Html.div [
        prop.classes ["tabs"; "is-large"; "is-boxed"; "is-toggle"; "is-fullwidth"]
        prop.children [
            Html.ul [
                Html.li [
                    className General |> prop.className
                    prop.children [Html.a [prop.href "#"
                                           prop.onClick (fun _ -> SwitchTab General |> dispatch)
                                           prop.text "General"]]]
                Html.li [
                    className LanguageIndependent |> prop.className
                    prop.children [Html.a [prop.href "#"
                                           prop.onClick (fun _ -> SwitchTab LanguageIndependent |> dispatch)
                                           prop.text "Common"]]]
                Html.li [
                    className LanguageDependent |> prop.className
                    prop.children [Html.a [prop.href "#"
                                           prop.onClick (fun _ -> SwitchTab LanguageDependent |> dispatch)
                                           prop.text "Per Language"]]]]]]

let inputText (id: string) (label: string) (value: string) (onChange: string -> unit) =
    Html.div [
        sprintf "div-just-for-%s" id |> prop.id
        prop.children [
            Html.label [
                prop.htmlFor id
                prop.text label]
            Html.input [
                prop.type' "text"
                prop.value value
                prop.onTextChange onChange]]]

let renderGeneralTab (state: State) (dispatch: Msg -> unit) =
    let updateConfigurationProjectName newName =
        SetProjectName newName |> dispatch
    let languageWidget (languageName: string) =
        Html.div [prop.text languageName]
    ((inputText "configuration-project-name" "Configuration Project Name"
                   state.Configuration.Name
                   updateConfigurationProjectName)
         ::List.map (fun x -> languageWidget x) (state.Configuration.Languages)
     ) |> Html.div

let limitingToInt32OnChange (setInt32: int -> Msg) (onChange: Msg -> unit) (newValue: int): unit =
    if (System.Int32.MinValue <= newValue) && (newValue <= System.Int32.MaxValue)
    then newValue |> setInt32 |> onChange
    else ()

let renderParameterInput
        (p: MinimalParameter)
        (id: string)
        (setString: string -> Msg)
        (setInt32: int -> Msg)
        (setBool: bool -> Msg)
        (onChange: Msg -> unit) =
    match (p :> IValidatableParameter).ValueAndDefault with
    | I { Value = v } ->
        Html.input [prop.id id
                    prop.value v
                    prop.type' "number"
                    limitingToInt32OnChange setInt32 onChange |> prop.onChange]
    | S { Value = s } ->
        Html.input [prop.id id
                    prop.value s
                    prop.type' "text"
                    setString >> onChange |> prop.onTextChange]
    | B { Value = b } ->
        Html.input [prop.id id
                    prop.value (p :> INamedParameter).Name
                    prop.type' "checkbox"
                    if b then prop.attributeName "checked"
                    setBool >> onChange |> prop.onCheckedChange]

let renderIndependentTab (state: State) (dispatch: Msg -> unit) =
    let renderLabelAndInput (p: IndependentParameter) =
        let name = (p :> INamedParameter).Name
        let pid = sprintf "lang-independent-%s" name
        Html.div [
            sprintf "div-for-%s" pid |> prop.id
            prop.children [
                Html.label [
                    prop.htmlFor pid
                    prop.text p.Description]
                renderParameterInput (new MinimalParameter(p))
                                     pid
                                     (fun s -> SetString (name, None, s))
                                     (fun i -> SetInt32 (name, None, i))
                                     (fun b -> SetBool (name, None, b))
                                     dispatch]]
    List.map renderLabelAndInput state.Configuration.Parameters |> Html.div

let renderValidationErrors (state: State) (dispatch: Msg -> unit) =
    let children = match state.ValidationErrors with
                   | [] -> [Html.div [prop.text "No validation errors."]]
                   | xs -> List.map (fun (x: LinkedMessage) -> Html.div [
                                         Html.a [prop.href "#"
                                                 prop.onClick (fun _ -> SwitchTab x.Tab |> dispatch)
                                                 prop.text x.Message]])
                                    xs
    Html.footer [prop.className "footer"
                 prop.children children]

let renderDependentTab (state: State) (dispatch: Msg -> unit) =
    let header = List.map (fun (s: string) -> Html.th [prop.text s]) (" "::(state.Configuration.Languages))
    let enumLanguages = enumerateList 0 state.Configuration.Languages
    let makeRow (p: LanguageParameter) =
        let firstCell = Html.th [prop.text p.Description]
        let makeCell (i, language) =
            let name = (p :> INamedParameter).Name
            let input =
                renderParameterInput (new MinimalParameter(p, i, language))
                                     (sprintf "%s-%d" name i)
                                     (fun s -> SetString (name, Some language, s))
                                     (fun i -> SetInt32 (name, Some language, i))
                                     (fun b -> SetBool (name, Some language, b))
                                     dispatch
            Html.td [input]
        firstCell::List.map makeCell enumLanguages |> Html.tr
    let rows = List.map makeRow state.Configuration.LanguageDependent
    Html.table [
        Html.thead [Html.tr header]
        Html.tbody rows]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        renderTabHeader state.ActiveTab dispatch
        match state.ActiveTab with
        | General -> renderGeneralTab state dispatch
        | LanguageIndependent -> renderIndependentTab state dispatch
        | LanguageDependent -> renderDependentTab state dispatch
        renderValidationErrors state dispatch
    ]

let init() =
    let state = { ActiveTab = General
                  Configuration = newConfiguration()
                  ValidationErrors = [] }
    { state with ValidationErrors = validateState state }

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
