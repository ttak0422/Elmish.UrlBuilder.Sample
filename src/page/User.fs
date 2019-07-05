module Sample.Page.User

open Elmish
open Fable.React
open Fable.React.Props
open GitHub
open Sample.Route

// Type

type State =
    | Init
    | Loaded of Repo list
    | Failed of exn

type Model =
    { State : State }

type Msg =
    | Receive of Result<Repo list, string>
    | ReceiveErr of exn

// State

let init userName =
    let model = { State = Init }
    let cmd = Cmd.OfAsync.either getRepos userName Receive ReceiveErr
    model, cmd

let update msg model : Model * Cmd<Msg> =
    match msg with
    | Receive (Ok repos) ->
        { model with State = Loaded repos }, Cmd.none
    | Receive (Error e) ->
        { model with State = Failed <| failwithf "%s" e }, Cmd.none
    | ReceiveErr e ->
        { model with State = Failed e }, Cmd.none

// View

let root model dispatch =
    let viewLink path label =
        li [] [ a [ Href path ] [ str label ] ]
    match model.State with
    | Init -> str "loading..."
    | Failed e -> str <| string e
    | Loaded repos ->
        ul []
            ( repos
            |> List.map (fun repo ->
                viewLink
                    (Route.toHash <| Repo (repo.Owner, repo.Name))
                    (repo.Owner + "/" + repo.Name)))