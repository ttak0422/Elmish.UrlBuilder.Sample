module Sample.View

open Elmish
open Fulma
open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Types
open Route

let root (model : Model) dispatch =
    let navbarBrand =
        Navbar.Brand.a []
            [ Navbar.Item.a 
                [ Navbar.Item.Props [ Href <| Route.toHash Top ] ]
                [ str "Sample Application" ] ]

    let navbarEnd =
        Navbar.End.div []
            [ Navbar.Item.div []
                [ Field.div [ Field.IsGrouped ]
                    [ Control.p []
                        [ Button.a [ Button.Props [ Href "https://github.com/ttak0422/Elmish.UrlBuilder"
                                                    Target "_blank" ] ]
                            [ Icon.icon []
                                [ Fa.i [ Fa.Brand.Github
                                         Fa.Size Fa.FaLarge ]
                                    [] ]
                              span [] [ str "GitHub(UrlBuilder)" ] ] ] 
                      Control.p []
                        [ Button.a [ Button.Props [ Href "https://github.com/ttak0422/Elmish.UrlBuilder.Sample"
                                                    Target "_blank" ] ]
                            [ Icon.icon []
                                [ Fa.i [ Fa.Brand.Github
                                         Fa.Size Fa.FaLarge ]
                                    [] ]
                              span [] [ str "GitHub(This Sample Application)" ] ] ] ] ] ]

    let navbar =
        Navbar.navbar [ Navbar.Color IsDark ]
            [ Container.container []
                [ navbarBrand
                  navbarEnd ] ]

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

    div 
        [] 
        [ navbar
          body ]
