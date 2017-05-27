Unofficial [Dropbox](https://www.dropbox.com) API for [Elm](http://elm-lang.org/).


## Demo

<https://avh4.github.io/elm-dropbox/>


## Usage

The Dropbox API uses OAuth 2.0 for authentication.
To authenticate the user, you will need to send them to the approrpiate authentication URL,
which can be done with `Dropbox.authorize`.
But to create the authorization URL, you will need to provide the `Navigation.Location` of the current page.

First, set up your `Model` to store the current page's location:

```sh
elm-package install elm-lang/navigation
```

```elm
import Navigation

type alias Model =
    { ...
    , location : Navigation.Location
    }

initialModel : Navigation.Location -> Model
initialModel location =
    { ...
    , location = location
    }
```

Dropbox authentication requires a "client id", also called the "app key".
can be found on the Dropbox Developers page for your app. Links: your Dropbox apps, creat a new Dropbox app
You will need to get the client id/app key of your [existing app](https://www.dropbox.com/developers/apps),
or [create a new app](https://www.dropbox.com/developers/apps/create).

Now, you can create a `Msg` that will initiate Dropbox login:

```sh
elm-package install avh4/elm-dropbox
```

```elm
import Dropbox

type Msg
    = ...
    | LogInToDropbox

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ...
        LogInToDropbox ->
            ( model
            , Dropbox.authorize
                { clientId = "MY_CLIENT_ID"
                , state = Nothing
                , requireRole = Nothing
                , forceReapprove = False
                , disableSignup = False
                , locale = Nothing
                , forceReauthentication = False
                }
                model.location
            )

view : Model -> Html Msg
view model =
    ...
    Html.button
        [ Html.Events.onClick LogInToDropbox ]
        [ Html.text "Log in with Dropbox" ]
```

When authentication completes, Dropbox will redirect to the current page,
providing the authentication details in the URL's fragment identifier .
You can use `Dropbox.program` to automatically parse the redirect response
(it also will help you get the `Navigation.Location` to store in your model).


```elm
import Dropbox

type Msg
    = ...
    | AuthResponse Dropbox.AuthorizeResult

main : Program Never Model (Dropbox.Msg Msg)
main =
    Dropbox.program
        { init = \location -> ( initialModel location, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onAuth = AuthResponse
        }
```

You can now handle the auth response and take the resulting `Dropbox.UserAuth` and use it to make API requests.
See the [documentation](http://package.elm-lang.org/packages/avh4/elm-dropbox/latest/Dropbox) for the full list of supported API calls.
Typically you will take the `Dropbox.UserAuth` and store it in your model for later use.
You may also want to persist it in local storage so that it can be used on future visits to your app.

```elm
import Dropbox

type alias Model =
    { ...
    , dropboxAuth : Maybe Dropbox.UserAuth
    }

type Msg
    = ...
    | FetchFileResponse (Result Dropbox.DownloadError Dropbox.DownloadResponse)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ...
        AuthResponse (Dropbox.AuthorizeOk auth) ->
            ( { model | dropboxAuth = Just auth.userAuth }
            , Dropbox.download auth.userAuth
                { path : "/data_file_to_load.json" }
                |> Task.attempt FetchFileResponse
            )
```
