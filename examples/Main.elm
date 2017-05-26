module Main exposing (..)

import BeautifulExample
import Color
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
    , auth : Maybe Dropbox.Auth
    }


initialModel : Navigation.Location -> Model
initialModel location =
    { debug = ""
    , writeFilename = "/elm-dropbox-test.txt"
    , writeContent = ""
    , config =
        Dropbox.configFromLocation
            "CLIENT_ID"
            location
    , auth = Nothing
    }


type Msg
    = StartAuth
    | Authed Dropbox.Auth
    | WriteFile Dropbox.Auth
    | ReadFile Dropbox.Auth
    | DebugResult String
    | ChangeWriteFilename String
    | ChangeAppId String
    | Logout Dropbox.Auth
    | LogoutResponse (Result Http.Error ())


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
            ( { model | auth = Just auth }
            , Cmd.none
            )

        WriteFile auth ->
            ( model
            , Dropbox.upload auth
                { filename = model.writeFilename
                , content = "HELLO."
                }
                |> Http.send (toString >> DebugResult)
            )

        ReadFile auth ->
            ( model
            , Dropbox.download auth
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
    div []
        [ h2 [] [ text "Dropbox.Config" ]
        , input [ onInput ChangeAppId, defaultValue model.config.clientId ] []
        , hr [] []
        , button
            [ onClick StartAuth ]
            [ text "Auth" ]
        , code []
            [ text <| Dropbox.authUrl model.config ]
        , hr [] []
        , code [] [ text <| toString model.auth ]
        , hr [] []
        , case model.auth of
            Just auth ->
                section []
                    [ input
                        [ onInput ChangeWriteFilename
                        , defaultValue model.writeFilename
                        ]
                        []
                    , button
                        [ onClick (WriteFile auth) ]
                        [ text "Write" ]
                    , hr [] []
                    , button
                        [ onClick (ReadFile auth) ]
                        [ text "Read" ]
                    , hr [] []
                    , button
                        [ onClick (Logout auth) ]
                        [ text "Log out" ]
                    ]

            Nothing ->
                text ""
        , code []
            [ text model.debug ]
        ]


main : Program Never Model (Maybe Msg)
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
                    , color = Just Color.lightBlue
                    , maxWidth = 600
                    , githubUrl = Just "https://github.com/avh4/elm-dropbox"
                    , documentationUrl = Nothing
                    }
        , onAuth = Authed
        }
