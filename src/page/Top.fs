module Sample.Page.Top

open Elmish
open Fable.React
open Fable.Core.JsInterop
open Fable.React.Props
open GitHub
open Sample.Route

// Type

type LoadingStatus =
   | Init
   | Waiting
   | Loaded of User list
   | Failed of exn

type Model =
    { Input : string
      Status : LoadingStatus }

type Msg =
    | Input of string
    | Send
    | Receive of Result<User list, string>
    | ReceiveErr of exn

// State

let init () =
    { Input = ""
      Status = Init }

let update msg model : Model * Cmd<Msg> =
    match msg with
    | Input input ->
        { model with Input = input }, Cmd.none
    | Send ->
        { model with
            Input = ""
            Status = Waiting }, Cmd.OfAsync.either searchUsers model.Input Receive ReceiveErr
    | Receive (Ok users) ->
        { model with Status = Loaded users}, Cmd.none
    | Receive (Error e) ->
        { model with Status = Failed <| failwithf "%s" e }, Cmd.none
    | ReceiveErr e ->
        { model with Status = Failed e}, Cmd.none

// View

let root model dispatch =
    let viewLink path label =
        li [] [ a [ Href path ] [ str label ] ]
    let viewUser user =
        viewLink (Route.toHash <| User user.Login) user.Login
    let viewUsers users =
        ul [] (List.map viewUser users)

    div []
        [ input
            [ OnInput (fun x -> x.target?value |> string |> Input |> dispatch)
              Value model.Input
              Placeholder "GitHub name"
              AutoFocus true ]
          button
            [ Disabled (model.Input.Length < 1)
              OnClick (fun _ -> dispatch Send) ]
            [ str "Send" ]
          (match model.Status with
           | Init -> str ""
           | Waiting -> str "waiting..."
           | Loaded users -> viewUsers users
           | Failed e -> str <| string e) ]
