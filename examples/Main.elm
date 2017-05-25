module Main exposing (..)

import Dropbox
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Navigation


type alias Model =
    { debug : String
    , writeFilename : String
    , writeContent : String
    , config : Dropbox.Config
    , auth : Dropbox.Auth
    }


initialModel : Model
initialModel =
    { debug = ""
    , writeFilename = "/elm-dropbox-test.txt"
    , writeContent = ""
    , config =
        { clientId = ""
        , redirectUri = ""
        }
    , auth =
        { accessToken = ""
        , tokenType = ""
        , uid = ""
        , accountId = ""
        }
    }


type Msg
    = StartAuth
    | Authed Dropbox.Auth
    | WriteFile
    | ReadFile
    | DebugResult String
    | ChangeWriteFilename String
    | ChangeAppId String
    | ChangeRedirectUrl String
    | ChangeAccessToken String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        config =
            model.config

        auth =
            model.auth
    in
    case msg of
        StartAuth ->
            ( model
            , Navigation.load <|
                Dropbox.authUrl model.config
            )

        Authed auth ->
            ( { model | auth = auth }
            , Cmd.none
            )

        WriteFile ->
            ( model
            , Dropbox.upload model.auth
                { filename = model.writeFilename
                , content = "HELLO."
                }
                |> Http.send (toString >> DebugResult)
            )

        ReadFile ->
            ( model
            , Dropbox.download model.auth
                { filename = model.writeFilename
                }
                |> Http.send (toString >> DebugResult)
            )

        DebugResult result ->
            ( { model | debug = result }
            , Cmd.none
            )

        ChangeWriteFilename filename ->
            ( { model | writeFilename = filename }
            , Cmd.none
            )

        ChangeAppId appId ->
            ( { model | config = { config | clientId = appId } }
            , Cmd.none
            )

        ChangeRedirectUrl url ->
            ( { model | config = { config | redirectUri = url } }
            , Cmd.none
            )

        ChangeAccessToken token ->
            ( { model | auth = { auth | accessToken = token } }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Dropbox.Config" ]
        , input [ onInput ChangeAppId, defaultValue model.config.clientId ] []
        , input [ onInput ChangeRedirectUrl, defaultValue model.config.redirectUri ] []
        , hr [] []
        , button
            [ onClick StartAuth ]
            [ text "Auth" ]
        , code []
            [ text <| Dropbox.authUrl model.config ]
        , input [ onInput ChangeAccessToken, defaultValue model.auth.accessToken ] []
        , hr [] []
        , if model.auth.accessToken /= "" then
            section []
                [ input
                    [ onInput ChangeWriteFilename
                    , defaultValue model.writeFilename
                    ]
                    []
                , button
                    [ onClick WriteFile ]
                    [ text "Write" ]
                , hr [] []
                , button
                    [ onClick ReadFile ]
                    [ text "Read" ]
                ]
          else
            text ""
        , code []
            [ text model.debug ]
        ]


main : Program Never Model (Maybe Msg)
main =
    Dropbox.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        , onAuth = Authed
        }
