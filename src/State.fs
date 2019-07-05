module Sample.State

open Elmish
open Types

let urlUpdate result model =
    match result with
    | None ->
        { model with Page = NotFound }, Cmd.none
    | Some Route.Top ->
        { model with Page = TopPage (Page.Top.init()) }, Cmd.none
    | Some (Route.User userName) ->
        let (userModel, userCmd) = Page.User.init userName
        { model with Page = UserPage userModel }, Cmd.map UserMsg userCmd
    | Some (Route.Repo (userName, projectName)) ->
        let (repoModel, repoCmd) = Page.Repo.init userName projectName
        { model with Page = RepoPage repoModel }, Cmd.map RepoMsg repoCmd

let init result =
    urlUpdate result { Page = TopPage (Page.Top.init()) }

let update msg model =
    match msg with
    | TopMsg topMsg ->
        match model.Page with
        | TopPage topModel ->
            let (newTopModel, topCmd) = Page.Top.update topMsg topModel
            { model with Page = TopPage newTopModel }, Cmd.map TopMsg topCmd
        | _ -> model, Cmd.none
    | UserMsg userMsg->
        match model.Page with
        | UserPage userModel ->
            let (newUserModel, userCmd) = Page.User.update userMsg userModel
            { model with Page = UserPage newUserModel }, Cmd.map UserMsg userCmd
        | _ -> model, Cmd.none
    | RepoMsg repoMsg ->
        match model.Page with
        | RepoPage repoModel ->
            let (newRepoModel, repoCmd) = Page.Repo.update repoMsg repoModel
            { model with Page = RepoPage newRepoModel }, Cmd.map RepoMsg repoCmd
        | _ -> model, Cmd.none
