module Sample.Page.Top

open Elmish
open Fulma
open Fable.React
open Fable.Core.JsInterop
open Fable.React.Props
open Fable.FontAwesome
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
let init() =
    { Input = ""
      Status = Init }

let update msg model : Model * Cmd<Msg> =
    match msg with
    | Input input -> { model with Input = input }, Cmd.none
    | Send ->
        { model with Input = ""
                     Status = Waiting },
        Cmd.OfAsync.either searchUsers model.Input Receive ReceiveErr
    | Receive(Ok users) -> { model with Status = Loaded users }, Cmd.none
    | Receive(Error e) ->
        { model with Status = Failed <| failwithf "%s" e }, Cmd.none
    | ReceiveErr e -> { model with Status = Failed e }, Cmd.none

// View
let root model dispatch =
    let onEnter msg dispatch =
        function
        | (e : Browser.Types.KeyboardEvent) when e.keyCode = 13. -> dispatch msg
        | _ -> ()
        |> OnKeyDown

    let searchComponent =
        let label = Label.label [] [ str "User Name" ]

        let input =
            Control.div [] [ Input.text [ Input.Props [ onEnter Send dispatch ]
                                          Input.Placeholder "Ex: elmish"
                                          Input.ValueOrDefault model.Input
                                          Input.OnChange(fun e ->
                                              !!e.target?value
                                              |> Input
                                              |> dispatch) ] ]

        let button =
            Control.div []
                [ Button.button [ Button.Color IsPrimary
                                  Button.IsLoading(model.Status = Waiting)
                                  Button.OnClick(fun _ -> dispatch Send) ]
                      [ str "Send" ] ]

        Field.div []
            [ label

              Level.level []
                  [ Level.left []
                        [ Level.item []
                              [ Field.div [ Field.HasAddons ] [ input; button ] ] ] ] ]

    let viewLink path label = li [] [ a [ Href path ] [ str label ] ]
    let viewUser (user : User) =
        viewLink (Route.toHash <| User user.Login) user.Login
    let viewUsers users = ul [] (List.map viewUser users)

    let userComponent users =
        let p text =
            Text.p [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ]
                [ str text ]
        match users with
        | [] ->
            Text.p [ Modifiers [ Modifier.TextWeight TextWeight.Bold
                                 Modifier.TextColor IsDanger ] ]
                [ str "Not Found" ]
        | [ user ] ->
            div [] [ p "Result"
                     ul [] [ viewUser user ] ]
        | users ->
            div [] [ p "Reults"
                     viewUsers users ]

    let pageComponent =
        match model.Status with
        | Init
        | Waiting -> str ""
        | Loaded users -> userComponent users
        | Failed e -> str <| string e

    div [] [ searchComponent; pageComponent ]
