module Sample.Route

open Elmish.UrlParser
open Elmish.UrlBuilder

type UserName = string

type ProjectName = string

type Route =
    | Top
    | User of UserName
    | Repo of UserName * ProjectName

module Route =
    let toHash route =
        match route with
        | Top -> Builder.relative [] []
        | User userName -> Builder.relative [ "user"; userName ] []
        | Repo(userName, projectName) ->
            Builder.relative [ "repo"; userName; projectName ] []
        |> fun url -> "#" + url

let parser : Parser<Route -> Route, Route> =
    let curry f x y = f (x, y)
    oneOf [ map Top top
            map User (s "user" </> str)
            map (curry Repo) (s "repo" </> str </> str) ]
