module Sample.Types

open GitHub

type Page =
    | NotFound
    | TopPage of Page.Top.Model
    | UserPage of Page.User.Model
    | RepoPage of Page.Repo.Model

type Model =
    { Page : Page }

type Msg =
    | TopMsg of Page.Top.Msg
    | UserMsg of Page.User.Msg
    | RepoMsg of Page.Repo.Msg
