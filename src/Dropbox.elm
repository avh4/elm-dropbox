module Dropbox exposing (Auth, Config, DownloadResponse, UploadResponse, authUrl, download, upload)

import Http
import Json.Decode
import Json.Encode


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


type alias DownloadResponse =
    { content : String }


download : Auth -> { filename : String } -> Http.Request DownloadResponse
download auth info =
    let
        url =
            "https://content.dropboxapi.com/2/files/download"

        decoder =
            Json.Decode.succeed {}

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
