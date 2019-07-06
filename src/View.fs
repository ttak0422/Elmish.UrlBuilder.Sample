module Sample.View

open Elmish
open Fulma
open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Types
open Route

let root (model : Model) dispatch =
    let header =
        Navbar.navbar [ Navbar.Color IsDark ]
            [ Navbar.Brand.a []
                  [ Navbar.Item.a [ Navbar.Item.Props [ Href "/" ] ]
                        [ str "Sample Application" ] ] ]

    let breadcrumb =
        match model.Page with
        | NotFound
        | TopPage _ ->
            let hrefHome = Route.toHash Top
            Breadcrumb.breadcrumb []
                [ Breadcrumb.item [] [ a [ Href hrefHome ] [ Fa.i
                                                                 [ Fa.Solid.Home ]
                                                                 []
                                                             str "Home" ] ] ]
        | UserPage userModel ->
            let hrefHome = Route.toHash Top
            let hrefUser = Route.toHash <| User userModel.UserName
            Breadcrumb.breadcrumb []
                [ Breadcrumb.item [] [ a [ Href hrefHome ] [ Fa.i
                                                                 [ Fa.Solid.Home ]
                                                                 []
                                                             str "Home" ] ]
                  Breadcrumb.item [] [ a [ Href hrefUser ] [ Fa.i
                                                                 [ Fa.Solid.User ]
                                                                 []
                                                             str "User" ] ] ]
        | RepoPage repoModel ->
            let hrefHome = Route.toHash Top
            let hrefUser = Route.toHash <| User repoModel.UserName
            let hrefRepo =
                Route.toHash <| Repo(repoModel.UserName, repoModel.ProjectName)
            Breadcrumb.breadcrumb []
                [ Breadcrumb.item [] [ a [ Href hrefHome ] [ Fa.i
                                                                 [ Fa.Solid.Home ]
                                                                 []
                                                             str "Home" ] ]
                  Breadcrumb.item [] [ a [ Href hrefUser ] [ Fa.i
                                                                 [ Fa.Solid.User ]
                                                                 []
                                                             str "User" ] ]

                  Breadcrumb.item []
                      [ a [ Href hrefRepo ] [ Fa.i [ Fa.Solid.Book ] []
                                              str "Repository" ] ] ]

    let viewNotFound = str "not found"

    let pageview =
        match model.Page with
        | NotFound -> viewNotFound
        | TopPage topPageModel ->
            Page.Top.root topPageModel (TopMsg >> dispatch)
        | UserPage userPageModel ->
            Page.User.root userPageModel (UserMsg >> dispatch)
        | RepoPage repoPageModel ->
            Page.Repo.root repoPageModel (RepoMsg >> dispatch)

    let body =
        Container.container [] [ br []
                                 breadcrumb
                                 pageview ]

    div [] [ header; body ]
