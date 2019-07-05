module Sample

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Elmish.React
open Sample.Route
open Sample.State
open Sample.View

Program.mkProgram init update root
|> Program.toNavigable (parseHash parser) urlUpdate
|> Program.withReactBatched "elmish-app"
|> Program.run