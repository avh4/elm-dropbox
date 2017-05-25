module Main exposing (..)

import Dict
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


parseAuth : String -> Maybe Dropbox.Auth
parseAuth string =
    let
        isKeyValue list =
            case list of
                [ k, v ] ->
                    Just ( k, v )

                _ ->
                    Nothing

        makeAuth dict =
            Maybe.map4 Dropbox.Auth
                (Dict.get "access_token" dict)
                (Dict.get "token_type" dict)
                (Dict.get "uid" dict)
                (Dict.get "account_id" dict)
    in
    case String.uncons string of
        Just ( '#', hash ) ->
            hash
                |> String.split "&"
                |> List.map (String.split "=")
                |> List.filterMap isKeyValue
                |> Dict.fromList
                |> makeAuth

        _ ->
            Nothing


main : Program Never Model (Maybe Msg)
main =
    Navigation.program (always Nothing)
        { init =
            \location ->
                case parseAuth location.hash of
                    Nothing ->
                        ( initialModel, Cmd.none )

                    Just auth ->
                        ( { initialModel | auth = auth }
                        , Cmd.none
                        )
        , update =
            \msg model ->
                case msg of
                    Nothing ->
                        ( model, Cmd.none )

                    Just m ->
                        update m model
                            |> Tuple.mapSecond (Cmd.map Just)
        , subscriptions = \_ -> Sub.none
        , view = view >> Html.map Just
        }
