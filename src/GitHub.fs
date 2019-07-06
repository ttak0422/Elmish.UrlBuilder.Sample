module GitHub

open Fable.SimpleHttp
open Thoth.Json
open Elmish.UrlBuilder.Builder

[<LiteralAttribute>]
let GITHUB_API_URL = "https://api.github.com"

type Repo =
    { Name : string
      Description : Option<string>
      Language : Option<string>
      Owner : string
      Fork : int
      Star : int
      Watch : int }

    static member Decode : Decoder<_> =
        Decode.object (fun get ->
            { Name = get.Required.Field "name" Decode.string
              Description = get.Optional.Field "descripiton" Decode.string
              Language = get.Optional.Field "language" Decode.string
              Owner = get.Required.At [ "owner"; "login" ] Decode.string
              Fork = get.Required.Field "forks_count" Decode.int
              Star = get.Required.Field "stargazers_count" Decode.int
              Watch = get.Required.Field "watchers_count" Decode.int })

    static member DecodeList : Decoder<_> = Decode.list Repo.Decode

type Issue =
    { Number : int
      Title : string
      State : string }

    static member Decode : Decoder<_> =
        Decode.object (fun get ->
            { Number = get.Required.Field "number" Decode.int
              Title = get.Required.Field "title" Decode.string
              State = get.Required.Field "state" Decode.string })

    static member DecodeList : Decoder<_> = Decode.list Issue.Decode

type User =
    { Login : string
      AvatarUrl : string }

    static member Decode : Decoder<_> =
        Decode.object (fun get ->
            { Login = get.Required.Field "login" Decode.string
              AvatarUrl = get.Required.Field "avatar_url" Decode.string })

    static member DecodeList : Decoder<_> = Decode.list User.Decode


let fetch url decoder =
    async {
        let! (status, res) = Http.get url
        return match status with
               | 200 -> Decode.fromString decoder res
               | x -> Error <| failwithf "Status : %d" x
    }

let getRepos userName =
    let url = crossOrigin GITHUB_API_URL [ "users"; userName; "repos" ] []
    fetch url Repo.DecodeList

let getIssues (userName, projectName) =
    let url =
        crossOrigin GITHUB_API_URL [ "repos"; userName; projectName; "issues" ]
            []
    fetch url Issue.DecodeList

let getUser userName =
    let url = crossOrigin GITHUB_API_URL [ "users"; userName ] []
    fetch url User.Decode

let searchUsers userName =
    let url =
        crossOrigin GITHUB_API_URL [ "search"; "users" ] [ str "q" userName ]
    fetch url (Decode.field "items" User.DecodeList)
