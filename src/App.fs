module App

open System
open Elmish
open Elmish.React
open Feliz

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

type State =
    { Count: int
      Text: string
      Parsed: Validated<int>
      KeepFocusOnText : bool
      ForceType : bool
    }

type Msg =
    | Increment
    | Decrement
    | SetTextInput of string
    | SetForceType of bool

let init() =
    let text = "empty (by init)"
    { Count = 0
      Text = text
      Parsed = { Parsed = None; Raw = text }
      KeepFocusOnText = false
      ForceType = true
    }

let update (msg: Msg) (state: State): State =
    match msg with
    | Increment ->
        { state with Count = state.Count + 1; KeepFocusOnText = false }

    | Decrement ->
        { state with Count = state.Count - 1; KeepFocusOnText = false }

    | SetTextInput newText ->
        let parsed = parseValidate int newText
        { (match parsed with
               | { Parsed = Some n } -> { state with Count = n }
               | _ -> state)
          with Text = newText; KeepFocusOnText = true; Parsed = parsed }

    | SetForceType forceType ->
        { state with ForceType = forceType }

let textHint (s: State) =
    match s with
        | { Parsed = { Parsed = Some _ } } -> "Overwrite counter value."
        | { Parsed = { Parsed = None; Raw = raw }; ForceType = false } when String.IsNullOrWhiteSpace raw = false ->
            raw.Trim() |> sprintf "Please enter a number instead of '%s'."
        | { Parsed = { Parsed = None } } -> "Please enter a number."

let validatedTextColor (m: Validated<int>) =
    match m with
        | { Parsed = None } -> color.red
        | _ -> color.green

let render (state: State) (dispatch: Msg -> unit) =
  let headerText =
      if state.Count % 2 = 0
      then "Count is even"
      else "Count is odd"
  Html.div [
    Html.button [
      prop.onClick (fun _ -> dispatch Increment)
      prop.text "Increment"
    ]

    Html.button [
      prop.onClick (fun _ -> dispatch Decrement)
      prop.text "Decrement"
    ]

    Html.h1 state.Count

    if state.Count >= 5 then Html.h1 headerText

    Html.label [
      prop.htmlFor "checkboxId"
      prop.text "Force number type"
    ]

    Html.input [
      prop.id "checkboxId"
      prop.type'.checkbox
      prop.isChecked state.ForceType
      prop.onCheckedChange (SetForceType >> dispatch)
    ]
    Html.input [
      if state.ForceType then prop.type'.number else prop.type'.text
      prop.style [validatedTextColor state.Parsed |> style.color]
      prop.onChange (SetTextInput >> dispatch)
      prop.value state.Text
      prop.autoFocus state.KeepFocusOnText
    ]

    Html.div [
      prop.style [validatedTextColor state.Parsed |> style.color]
      textHint state  |> prop.text
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
