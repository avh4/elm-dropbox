module Dropbox exposing (Auth, Config, DownloadResponse, UploadResponse, authUrl, download, program, tokenRevoke, upload)

{-|


## Dropbox API

See the official Dropbox documentation at
<https://www.dropbox.com/developers/documentation/http/documentation>


### Auth

@docs tokenRevoke


### Files

@docs download, upload

-}

import Dict
import Html exposing (Html)
import Http
import Json.Decode
import Json.Encode
import Navigation
import Update.Extra


type alias Config =
    { clientId : String
    , redirectUri : String
    }


type alias Auth =
    { accessToken : String
    , tokenType : String
    , uid : String
    , accountId : String
    }


authUrl : Config -> String
authUrl config =
    String.concat
        [ "https://www.dropbox.com/oauth2/authorize"
        , "?"
        , "response_type=token"
        , "&"
        , "client_id="
        , config.clientId
        , "&"
        , "redirect_uri="
        , config.redirectUri
        ]


parseAuth : Navigation.Location -> Maybe Auth
parseAuth location =
    let
        isKeyValue list =
            case list of
                [ k, v ] ->
                    Just ( k, v )

                _ ->
                    Nothing

        makeAuth dict =
            Maybe.map4 Auth
                (Dict.get "access_token" dict)
                (Dict.get "token_type" dict)
                (Dict.get "uid" dict)
                (Dict.get "account_id" dict)
    in
    case String.uncons location.hash of
        Just ( '#', hash ) ->
            hash
                |> String.split "&"
                |> List.map (String.split "=")
                |> List.filterMap isKeyValue
                |> Dict.fromList
                |> makeAuth

        _ ->
            Nothing


tokenRevoke : Auth -> Http.Request ()
tokenRevoke auth =
    let
        url =
            "https://api.dropboxapi.com/2/auth/token/revoke"

        parse response =
            Ok ()
    in
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ auth.accessToken)
            ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse parse
        , timeout = Nothing
        , withCredentials = False
        }


type alias DownloadResponse =
    { content : String }


download : Auth -> { filename : String } -> Http.Request DownloadResponse
download auth info =
    let
        url =
            "https://content.dropboxapi.com/2/files/download"

        parse response =
            Ok { content = response.body }

        dropboxArg =
            Json.Encode.encode 0 <|
                Json.Encode.object
                    [ ( "path", Json.Encode.string info.filename ) ]
    in
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ auth.accessToken)
            , Http.header "Dropbox-API-Arg" dropboxArg
            ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse parse
        , timeout = Nothing
        , withCredentials = False
        }


type alias UploadResponse =
    {}


type alias UploadRequest =
    { filename : String
    , content : String
    }


upload : Auth -> UploadRequest -> Http.Request UploadResponse
upload auth info =
    let
        url =
            "https://content.dropboxapi.com/2/files/upload"

        body =
            Http.stringBody "application/octet-stream" info.content

        decoder =
            Json.Decode.succeed {}

        dropboxArg =
            Json.Encode.encode 0 <|
                Json.Encode.object
                    [ ( "path", Json.Encode.string info.filename ) ]
    in
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ auth.accessToken)
            , Http.header "Dropbox-API-Arg" dropboxArg
            ]
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


program :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    , onAuth : Auth -> msg
    }
    -> Program Never model (Maybe msg)
program config =
    Navigation.program (always Nothing)
        { init =
            \location ->
                case parseAuth location of
                    Nothing ->
                        config.init
                            |> Update.Extra.mapCmd Just

                    Just auth ->
                        config.init
                            |> Update.Extra.andThen
                                config.update
                                (config.onAuth auth)
                            |> Update.Extra.mapCmd Just
        , update =
            \msg model ->
                case msg of
                    Nothing ->
                        ( model, Cmd.none )

                    Just m ->
                        config.update m model
                            |> Update.Extra.mapCmd Just
        , subscriptions = \_ -> Sub.none
        , view = config.view >> Html.map Just
        }
