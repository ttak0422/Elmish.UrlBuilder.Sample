module Sample.View

open Elmish
open Fable.React
open Fable.React.Props
open System
open Types

let root model dispatch =
    let viewNotFound = str "not found"
    div []
        [ a [ Href "#" ] [ h1[] [ str "My GitHub Viewer" ] ]
          (match model.Page with
          | NotFound -> viewNotFound
          | TopPage topPageModel -> Page.Top.root topPageModel (TopMsg >> dispatch)
          | UserPage userPageModel -> Page.User.root userPageModel (UserMsg >> dispatch)
          | RepoPage repoPageModel -> Page.Repo.root repoPageModel (RepoMsg >> dispatch)) ]

