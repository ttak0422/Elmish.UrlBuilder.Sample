module Sample.View

open Elmish
open Fulma
open Fable.React
open Fable.React.Props
open Types

let root model dispatch =
    let header =
        Navbar.navbar
            [ Navbar.Color IsDark ]
            [ Navbar.Brand.a [ ] [ Navbar.Item.a [] [ str "Sample Application" ] ] ]

    let viewNotFound = str "not found"
    div []
        [ header
          a [ Href "#" ] [ h1[] [ str "My GitHub Viewer" ] ]
          (match model.Page with
          | NotFound -> viewNotFound
          | TopPage topPageModel -> Page.Top.root topPageModel (TopMsg >> dispatch)
          | UserPage userPageModel -> Page.User.root userPageModel (UserMsg >> dispatch)
          | RepoPage repoPageModel -> Page.Repo.root repoPageModel (RepoMsg >> dispatch)) ]

