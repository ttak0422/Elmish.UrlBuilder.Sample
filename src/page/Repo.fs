module Sample.Page.Repo

open Elmish
open Fulma
open Fable.React
open Fable.React.Props
open GitHub

// Type
type State =
    | Init
    | Loaded of Issue list
    | Failed of exn

type Model =
    { UserName : string
      ProjectName : string
      State : State }

type Msg =
    | Receive of Result<Issue list, string>
    | ReceiveErr of exn

// State
let init userName projectName =
    let model =
        { UserName = userName
          ProjectName = projectName
          State = Init }

    let cmd =
        Cmd.OfAsync.either getIssues (userName, projectName) Receive ReceiveErr
    model, cmd

let update msg model : Model * Cmd<Msg> =
    match msg with
    | Receive(Ok issues) -> { model with State = Loaded issues }, Cmd.none
    | Receive(Error e) ->
        { model with State = Failed <| failwithf "%s" e }, Cmd.none
    | ReceiveErr e -> { model with State = Failed e }, Cmd.none

// View
let root model dispatch =
    let viewIssues userName projectName (issue : Issue) =
        li []
            [ span [] [ str <| "[" + issue.State + "]" ]

              a [ Href
                  <| "https://github.com/" + userName + "/" + projectName
                     + "/issues/" + (string issue.Number)
                  Target "_blank" ]
                  [ str <| sprintf "#%d %s" issue.Number issue.Title ] ]
    match model.State with
    | Init -> Utils.loader
    | Failed e -> str <| string e
    | Loaded issues ->
        let projectName =
            Box.box' [ Modifiers [ Modifier.BackgroundColor IsPrimary ] ]
                [ Text.p
                      [ Modifiers
                            [ Modifier.TextWeight TextWeight.Bold
                              Modifier.TextSize(Screen.All, TextSize.Is3) ] ]
                      [ str <| sprintf "%s/%s" model.UserName model.ProjectName ] ]
        Box.box' []
            [ projectName

              Text.p [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ]
                  [ str "Issues" ]

              ul []
                  (List.map (viewIssues model.UserName model.ProjectName) issues) ]
