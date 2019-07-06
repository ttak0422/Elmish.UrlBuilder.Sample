module Sample.Page.Repo

open Elmish
open Elmish.UrlBuilder
open Fulma
open Fable.React
open Fable.React.Props
open GitHub
open Sample.Route

// Type
type State =
    | Init
    | Loaded of Issue list
    | Failed of exn

type Model =
    { UserName : UserName
      ProjectName : ProjectName
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
    let viewIssue userName projectName (issue : Issue) =
        let url = Builder.crossOrigin "https://github.com" [ userName; projectName; "issues"; string issue.Number ] []
        li []
            [ span [] [ str <| "[" + issue.State + "]" ]
              a [ Href url
                  Target "_blank" ]
                  [ str <| sprintf "#%d %s" issue.Number issue.Title ] ]
    let viewIssues userName projectName (issues : Issue list) =
        let viewIssue = viewIssue userName projectName
        ul [] (List.map viewIssue issues)
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
        let issues =
            let p text = Text.p [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ] [str text ]
            match issues with
            | [] -> p "There are no issues."
            | [ issue ] ->
                div []
                  [ p "Issue"
                    viewIssue model.UserName model.ProjectName issue ]

            | issues ->
                div []
                  [ p "Issues"
                    viewIssues model.UserName model.ProjectName issues ]
        Box.box' []
            [ projectName
              issues ]
