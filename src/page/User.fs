module Sample.Page.User

open Elmish
open Fulma
open Fable.React
open Fable.React.Props
open GitHub
open Sample.Route

// Type
type Status =
    | Init
    | Loaded of (User * Repo list)
    | Failed of exn

type Model =
    { Status : Status
      UserName : UserName }

type Msg =
    | Receive of Result<User * Repo list, exn>
    | ReceiveErr of exn

// State
let init userName =
    let get() =
        async {
            let! user = getUser userName
            let! repo = getRepos userName
            match (user, repo) with
            | (Ok user, Ok repos) -> return Ok(user, repos)
            | _ -> return failwith "failed"
        }

    let model =
        { Status = Init
          UserName = userName }

    model, Cmd.OfAsync.either get () Receive ReceiveErr

let update msg model : Model * Cmd<Msg> =
    match msg with
    | Receive(Ok(user, repos)) ->
        { model with Status = Loaded(user, repos) }, Cmd.none
    | Receive(Error e)
    | ReceiveErr e -> { model with Status = Failed e }, Cmd.none

// View
let root model dispatch =
    let viewLink path label = li [] [ a [ Href path ] [ str label ] ]
    let viewRepo repo =
        viewLink (Route.toHash <| Repo(repo.Owner, repo.Name)) repo.Name
    let viewRepos repos = ul [] (List.map viewRepo repos)

    let repoComponent repos =
        let p text =
            Text.p [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ]
                [ str text ]
        match repos with
        | [] ->
            Text.p [ Modifiers [ Modifier.TextWeight TextWeight.Bold
                                 Modifier.TextColor IsDanger ] ]
                [ str "There are no repository." ]
        | [ repo ] ->
            div [] [ p "Repository"
                     ul [] [ viewRepo repo ] ]
        | repos ->
            div [] [ p "Repositories"
                     viewRepos repos ]
    match model.Status with
    | Init -> Utils.loader
    | Failed e -> str <| string e
    | Loaded(user, repos) ->
        let user =
            Box.box' [ Modifiers [ Modifier.BackgroundColor IsPrimary ] ]
                [ Media.media []
                      [ Media.left []
                            [ Image.image [ Image.Is128x128 ]
                                  [ img [ Src user.AvatarUrl ] ] ]

                        Media.content []
                            [ Text.p
                                  [ Modifiers
                                        [ Modifier.TextAlignment
                                              (Screen.All,
                                               TextAlignment.Justified)
                                          Modifier.TextWeight TextWeight.Bold

                                          Modifier.TextSize
                                              (Screen.All, TextSize.Is2) ] ]
                                  [ str user.Login ] ] ] ]
        Box.box' [] [ user
                      repoComponent repos ]
