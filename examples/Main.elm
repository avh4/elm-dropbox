module Main exposing (..)

import BeautifulExample
import Color
import Date exposing (Date)
import Debug.Control as Control exposing (Control)
import Dropbox
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Navigation
import Task


type alias Model =
    { debug : String
    , uploadRequest : Control Dropbox.UploadRequest
    , downloadRequest : Control Dropbox.DownloadRequest
    , clientId : String
    , location : Navigation.Location
    , auth : Maybe Dropbox.UserAuth
    }


initialModel : Navigation.Location -> Model
initialModel location =
    { debug = ""
    , uploadRequest =
        Control.record Dropbox.UploadRequest
            |> Control.field "path" (Control.string "/elm-dropbox-demo.txt")
            |> Control.field "mode"
                (Control.choice
                    [ ( "Add", Control.value Dropbox.Add )
                    , ( "Overwrite", Control.value Dropbox.Overwrite )
                    , ( "Update rev", Control.map Dropbox.Update <| Control.string "123abcdef" )
                    ]
                )
            |> Control.field "autorename" (Control.bool False)
            |> Control.field "clientModified"
                (Control.maybe False <| Control.date <| Date.fromTime 0)
            |> Control.field "mute" (Control.bool False)
            |> Control.field "content" (Control.string "HELLO.")
    , downloadRequest =
        Control.record Dropbox.DownloadRequest
            |> Control.field "path" (Control.string "/elm-dropbox-demo.txt")
    , clientId =
        if location.host == "avh4.github.io" then
            "cackwvfdggogoes"
        else
            ""
    , location = location
    , auth = Nothing
    }


type Msg
    = StartAuth
    | Authed Dropbox.AuthorizeResult
    | WriteFile Dropbox.UserAuth
    | ReadFile Dropbox.UserAuth
    | DebugResult String
    | ChangeUploadRequest (Control Dropbox.UploadRequest)
    | ChangeDownloadRequest (Control Dropbox.DownloadRequest)
    | ChangeAppId String
    | Logout Dropbox.UserAuth
    | LogoutResponse (Result Http.Error ())


authRequest : Model -> Dropbox.AuthorizeRequest
authRequest model =
    { clientId = model.clientId
    , state = Nothing
    , requireRole = Nothing
    , forceReapprove = False
    , disableSignup = False
    , locale = Nothing
    , forceReauthentication = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartAuth ->
            ( model
            , Dropbox.authorize (authRequest model) model.location
            )

        Authed (Dropbox.AuthorizeOk auth) ->
            { model | auth = Just auth.userAuth }
                |> update (DebugResult <| toString <| msg)

        Authed _ ->
            { model | auth = Nothing }
                |> update (DebugResult <| toString <| msg)

        WriteFile auth ->
            ( model
            , Dropbox.upload auth
                (Control.currentValue model.uploadRequest)
                |> Task.attempt (toString >> DebugResult)
            )

        ReadFile auth ->
            ( model
            , Dropbox.download auth
                (Control.currentValue model.downloadRequest)
                |> Task.attempt (toString >> DebugResult)
            )

        DebugResult result ->
            ( { model | debug = result }
            , Cmd.none
            )

        ChangeUploadRequest uploadRequest ->
            ( { model | uploadRequest = uploadRequest }
            , Cmd.none
            )

        ChangeDownloadRequest downloadRequest ->
            ( { model | downloadRequest = downloadRequest }
            , Cmd.none
            )

        ChangeAppId appId ->
            ( { model | clientId = appId }
            , Cmd.none
            )

        Logout auth ->
            ( model
            , Dropbox.tokenRevoke auth
                |> Http.send LogoutResponse
            )

        LogoutResponse (Ok ()) ->
            ( { model | auth = Nothing }
            , Cmd.none
            )

        LogoutResponse (Err err) ->
            update (DebugResult <| toString <| Err err) model


view : Model -> Html Msg
view model =
    [ [ h3 [] [ text "Step 1: provide your clientId" ]
      , p []
            [ text "First, you need to provide your Dropbox application's clientId. "
            , text "This is also called the \"App key\" and can be found on the Dropbox Developers page for your app. "
            , text "Links: "
            , a [ href "https://www.dropbox.com/developers/apps" ] [ text "your Dropbox apps" ]
            , text ", "
            , a [ href "https://www.dropbox.com/developers/apps/create" ] [ text "creat a new Dropbox app" ]
            , p [] [ text "In code, you will hard-code the clientId into your app's source code. " ]
            , p [] [ text "In this example, you will need to enter it below:" ]
            ]
      , input
            [ onInput ChangeAppId
            , defaultValue model.clientId
            , placeholder "clientId a.k.a., App key"
            ]
            []
      ]
    , if String.trim model.clientId == "" then
        []
      else
        [ h3 [] [ text "Step 2: redirect to the auth URL" ]
        , p []
            [ text "The Dropbox API uses OAuth 2.0 for user authentication. "
            , text "To initiate an authentication request, you must redirect the user to the auth URL. "
            , text "You can do that with the following code:"
            ]
        , pre [] [ text """startAuth : Cmd msg
startAuth =
    Dropbox.authorize
        { clientId = myClientId
        , state = Nothing
        , requireRole = Nothing
        , forceReapprove = False
        , disableSignup = False
        , locale = Nothing
        , forceReauthentication = False
        }
        model.location""" ]
        , p []
            [ text "For this example, the redirect URL is "
            , code
                [ style [ ( "word-break", "break-all" ) ] ]
                [ text <| Dropbox.authorizationUrl (authRequest model) (Dropbox.redirectUriFromLocation model.location) ]
            , text " You can redirect there using this button:"
            ]
        , button
            [ onClick StartAuth ]
            [ text "Auth" ]
        , p []
            [ text "When authentication is complete, Dropbox will redirect back to this page, "
            , text "providing the result in the URL, which can be parsed by using "
            , code [] [ text "Dropbox.program" ]
            , text "."
            ]
        ]
    , case model.auth of
        Just auth ->
            [ p []
                [ text "Auth token: "
                , code [] [ text <| toString model.auth ]
                ]
            , h3 [] [ text "Step 3: Use the API" ]
            , p []
                [ text "For details on the API calls, see the "
                , a [ href "https://www.dropbox.com/developers/documentation/http/documentation" ]
                    [ text "Dropbox API v2 documentation" ]
                ]
            , h4 [] [ text "files/upload" ]
            , Control.view ChangeUploadRequest model.uploadRequest
            , button
                [ onClick (WriteFile auth) ]
                [ text "Upload" ]
            , hr [] []
            , h4 [] [ text "files/download" ]
            , Control.view ChangeDownloadRequest model.downloadRequest
            , button
                [ onClick (ReadFile auth) ]
                [ text "download" ]
            , hr [] []
            , h4 [] [ text "auth/token_revoke" ]
            , button
                [ onClick (Logout auth) ]
                [ text "token_revoke" ]
            ]

        Nothing ->
            []
    , [ h3 [] [ text "Debug output" ]
      , code [] [ text model.debug ]
      ]
    ]
        |> List.filter ((/=) [])
        |> List.intersperse [ hr [] [] ]
        |> List.concat
        |> div []


main : Program Never Model (Dropbox.Msg Msg)
main =
    Dropbox.program
        { init = \location -> ( initialModel location, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view =
            view
                >> BeautifulExample.view
                    { title = "elm-dropbox"
                    , details =
                        Just """Unofficial Dropbox API for Elm."""
                    , color = Just <| Color.rgb 40 136 222
                    , maxWidth = 600
                    , githubUrl = Just "https://github.com/avh4/elm-dropbox"
                    , documentationUrl = Just "http://package.elm-lang.org/packages/avh4/elm-dropbox/latest"
                    }
        , onAuth = Authed
        }
