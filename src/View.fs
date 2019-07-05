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
            [ Navbar.Brand.a [ ] [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ] [ str "Sample Application" ] ] ]

    let viewNotFound = str "not found"
    div []
        [ header
          (match model.Page with
          | NotFound -> viewNotFound
          | TopPage topPageModel -> Page.Top.root topPageModel (TopMsg >> dispatch)
          | UserPage userPageModel -> Page.User.root userPageModel (UserMsg >> dispatch)
          | RepoPage repoPageModel -> Page.Repo.root repoPageModel (RepoMsg >> dispatch)) ]

