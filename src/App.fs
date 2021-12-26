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

type LanguageDependentParameters =
    { Language: string
      Parameters: Parameter list }

type Configuration =
    { Languages: LanguageDependentParameters list
      Parameters: Parameter list
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
    | SetInt32 of string * string Option * string
    | SetLanguageOrder of string list
    | SetProjectName of string

let reshuffleLanguages (data : LanguageDependentParameters list) (newLangs : string list) : LanguageDependentParameters list =
    match data with
        | [] -> List.map (fun lang -> { Language = lang; Parameters = []}) newLangs
        | { Parameters = dps }::_ ->
            let getLang lang =
                match List.tryFind (fun { Language = lg } -> lg = lang) data with
                    | Some x -> x
                    | None -> { Language = lang; Parameters = dps }
            List.map getLang newLangs

let validatedTextColor (m: Validated<int>) =
    match m with
        | { Parsed = None } -> color.red
        | _ -> color.green

let validateHasStringPrefix (prefix : string) (p : Parameter) =
    let rule ps =
        extractString p.Name ps
        |> applyOnOK (sprintf "Type error: '%s' is not a string." p.UiName)
                     (fun s -> if s.StartsWith(prefix)
                               then ""
                               else sprintf "'%s' should start with '%s'." p.UiName prefix)
    rule

let newConfiguration () =
    let mkLangParam t1 t2 = [{ Name = "s1"
                               UiName = "First string"
                               Description = "The first string to enter"
                               PerLanguage = true
                               ValidationRules = []
                               ValueAndDefault = S { Default = t1; Value = t1 } }
                             (let p = { Name = "s2"
                                        UiName = "Second string"
                                        Description = "The second string to enter"
                                        PerLanguage = true
                                        ValidationRules = []
                                        ValueAndDefault = S { Default = t2; Value = t2 } }
                              { p with ValidationRules = validateHasStringPrefix "s2" p::p.ValidationRules })]
    { Languages = [{ Language = "English"; Parameters = mkLangParam "1st value" "2nd value" }
                   { Language = "French"; Parameters = mkLangParam "1ère valeur" "2ème valeur" }]
      Parameters = []
      Name = "Demo configuration" }

let init() =
    { ActiveTab = General
      Configuration = newConfiguration ()
      ValidationErrors = []
    }

let updateNoValidate (msg: Msg) (state: State): State =
    match msg with
        | SwitchTab newTab -> { state with ActiveTab = newTab }
        | SetLanguageOrder newLangs ->
            let newDependents = reshuffleLanguages (state.Configuration.Languages) newLangs
            let newConfiguration = { state.Configuration with Languages = newDependents }
            { state with Configuration = newConfiguration }
        | SetProjectName newName -> { state with Configuration = { state.Configuration with Name = newName}}
        | _ -> state

let validateState state =
    if state.Configuration.Name.Trim() = ""
    then [{ Tab = General; Message = "Configuration Project Name may not be blank." }]
    else []

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
                prop.for' id
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
         ::List.map (fun x -> languageWidget x.Language) (state.Configuration.Languages)
     ) |> Html.div

let renderIndependentTab (state: State) (dispatch: Msg -> unit) =
    Html.div []

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

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        renderTabHeader state.ActiveTab dispatch
        match state.ActiveTab with
            | General -> renderGeneralTab state dispatch
            | LanguageIndependent -> Html.div [Html.p [prop.text "Language independent tab still to implement."]]
            | LanguageDependent -> Html.div [Html.p [prop.text "Language dependent tab still to implement."]]
        renderValidationErrors state dispatch
    ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
