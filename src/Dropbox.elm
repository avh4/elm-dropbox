module Dropbox
    exposing
        ( AuthorizeRequest
        , Dimensions
        , DownloadError(..)
        , DownloadRequest
        , DownloadResponse
        , FileSharingInfo
        , GpsCoordinates
        , LookupError(..)
        , MediaInfo
        , MediaMetadata
        , PhotoMetadata
        , PropertyGroup
        , UploadError(..)
        , UploadRequest
        , UploadResponse
        , UploadWriteFailed
        , UserAuth
        , VideoMetadata
        , WriteError(..)
        , WriteMode(..)
        , authFromLocation
        , authorizationUrl
        , authorize
        , download
        , program
        , tokenRevoke
        , upload
        )

{-|


## Dropbox API

See the official Dropbox documentation at
<https://www.dropbox.com/developers/documentation/http/documentation>

@docs program


### Authorization

@docs AuthorizeRequest, authFromLocation, authorize, authorizationUrl, UserAuth


### Auth

@docs tokenRevoke


### Files

@docs download, DownloadRequest, DownloadResponse, DownloadError, LookupError
@docs upload, UploadRequest, WriteMode, UploadResponse, UploadError, UploadWriteFailed, WriteError

@docs MediaInfo, MediaMetadata, PhotoMetadata, VideoMetadata, Dimensions, GpsCoordinates, FileSharingInfo, PropertyGroup

-}

import Date exposing (Date)
import Date.Format
import Dict exposing (Dict)
import Html exposing (Html)
import Http
import Json.Decode
import Json.Decode.Dropbox exposing (openUnion, optional, tagObject, tagValue, tagVoid, union)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline
import Json.Encode
import Navigation
import Task exposing (Task)
import Update.Extra


{-| Request parameters for Dropbox OAuth 2.0 authorization requests.

See <https://www.dropbox.com/developers/documentation/http/documentation#oauth2-authorize>

-}
type alias AuthorizeRequest =
    { clientId : String
    , redirectUri : String
    }


{-| Create a `AuthorizeRequest` from a `Navigation.Location`. This can be used
with `Navigation.program` to automatically generate the redirectUri from the
current page's URL.
-}
authFromLocation : String -> Navigation.Location -> AuthorizeRequest
authFromLocation clientId location =
    { clientId = clientId
    , redirectUri =
        location.protocol
            ++ "//"
            ++ location.host
            ++ location.pathname
    }


{-| Return value of the `authorize` endpoint, which is the data Dropbox returns via
the redirect URL.

You can get the `AuthorizeResponse` by using `Dropbox.program`,
or by using `parseAuth` if you need to manually parse the redirect URL.

See <https://www.dropbox.com/developers/documentation/http/documentation#oauth2-authorize>

-}
type alias AuthorizeResponse =
    { accessToken : String
    , tokenType : String
    , uid : String
    , accountId : String
    }


{-| The Dropbox OAuth 2.0 authorization URL.
Typically you will just want to use `authorize` instead,
which will initiate the authorization.

See <https://www.dropbox.com/developers/reference/oauth-guide>

-}
authorizationUrl : AuthorizeRequest -> String
authorizationUrl request =
    String.concat
        [ "https://www.dropbox.com/oauth2/authorize"
        , "?"
        , "response_type=token"
        , "&"
        , "client_id="
        , request.clientId
        , "&"
        , "redirect_uri="
        , request.redirectUri
        ]


{-| <https://www.dropbox.com/developers/documentation/http/documentation#oauth2-authorize>
-}
authorize : AuthorizeRequest -> Cmd msg
authorize request =
    Navigation.load <|
        authorizationUrl request


parseAuth : Navigation.Location -> Maybe AuthorizeResponse
parseAuth location =
    let
        isKeyValue list =
            case list of
                [ k, v ] ->
                    Just ( k, v )

                _ ->
                    Nothing

        makeAuth dict =
            Maybe.map4 AuthorizeResponse
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


{-| A user authentication token that can be used to authenticate API calls

See <https://www.dropbox.com/developers/reference/auth-types#user>

-}
type UserAuth
    = Bearer String


authorization : AuthorizeResponse -> Result String UserAuth
authorization response =
    case response.tokenType of
        "bearer" ->
            Ok <| Bearer response.accessToken

        _ ->
            Err ("Unknown token_type: " ++ response.tokenType)


authHeader : UserAuth -> Http.Header
authHeader auth =
    case auth of
        Bearer accessToken ->
            Http.header "Authorization" ("Bearer " ++ accessToken)


{-| Disables the access token used to authenticate the call.

See <https://www.dropbox.com/developers/documentation/http/documentation#auth-token-revoke>

-}
tokenRevoke : UserAuth -> Http.Request ()
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
            [ authHeader auth
            ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse parse
        , timeout = Nothing
        , withCredentials = False
        }


{-| Request parameteres for `download`

Note: there is no `rev` field because it is deprecated.
See <https://www.dropbox.com/developers/documentation/http/documentation#files-download>

-}
type alias DownloadRequest =
    { path : String
    }


{-| Return value for `download`

**WARNING**: elm-dropbox may give the incorrect values for `size`,
since Elm currently does not provide a way to parse and represent 64-bit integers.

-}
type alias DownloadResponse =
    { content : String
    , name : String
    , id : String
    , clientModified : Date
    , serverModified : Date
    , rev : String
    , size : Int -- XXX: should be UInt64
    , pathLower : Maybe String
    , pathDisplay : Maybe String
    , parentSharedFolderId : Maybe String
    , mediaInfo : Maybe MediaInfo
    , sharingInfo : Maybe FileSharingInfo
    , propertyGroups : Maybe (List PropertyGroup)
    , hasExplicitSharedMembers : Maybe Bool
    , contentHash : Maybe String
    }


decodeDownloadResponse : String -> Json.Decode.Decoder DownloadResponse
decodeDownloadResponse content =
    Pipeline.decode DownloadResponse
        |> Pipeline.hardcoded content
        |> Pipeline.required "name" Json.Decode.string
        |> Pipeline.required "id" Json.Decode.string
        |> Pipeline.required "client_modified" Json.Decode.Extra.date
        |> Pipeline.required "server_modified" Json.Decode.Extra.date
        |> Pipeline.required "rev" Json.Decode.string
        |> Pipeline.required "size" Json.Decode.int
        |> optional "path_lower" Json.Decode.string
        |> optional "path_display" Json.Decode.string
        |> optional "parent_shared_folder_id" Json.Decode.string
        |> optional "media_info" decodeMediaInfo
        |> optional "sharing_info" decodeFileSharingInfo
        |> optional "property_groups" (Json.Decode.list decodePropertyGroup)
        |> optional "has_explicit_shared_members" Json.Decode.bool
        |> optional "content_hash" Json.Decode.string


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-download>
-}
type DownloadError
    = PathDownloadError LookupError
    | OtherDownloadError String Json.Encode.Value
    | OtherDownloadFailure Http.Error


decodeDownloadError : Json.Decode.Decoder DownloadError
decodeDownloadError =
    Json.Decode.field "error" <|
        openUnion OtherDownloadError
            [ tagValue "path" PathDownloadError decodeLookupError
            ]


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-download>
-}
type LookupError
    = MalformedPathLookup (Maybe String)
    | NotFound
    | NotFile
    | NotFolder
    | RestrictedContent
    | OtherLookupError String Json.Encode.Value


decodeLookupError : Json.Decode.Decoder LookupError
decodeLookupError =
    openUnion OtherLookupError
        [ tagValue "malformed_path" MalformedPathLookup (Json.Decode.nullable Json.Decode.string)
        ]


{-| Download a file from a user's Dropbox.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-download>

-}
download : UserAuth -> DownloadRequest -> Task DownloadError DownloadResponse
download auth info =
    let
        url =
            "https://content.dropboxapi.com/2/files/download"

        parse response =
            case Dict.get "dropbox-api-result" response.headers of
                Nothing ->
                    Err "No dropbox-api-result header found"

                Just arg ->
                    Json.Decode.decodeString (decodeDownloadResponse response.body) arg

        dropboxArg =
            Json.Encode.encode 0 <|
                Json.Encode.object
                    [ ( "path", Json.Encode.string info.path ) ]

        decodeError err =
            case err of
                Http.BadStatus response ->
                    case Json.Decode.decodeString decodeDownloadError response.body of
                        Ok err ->
                            err

                        Err _ ->
                            OtherDownloadFailure err

                _ ->
                    OtherDownloadFailure err
    in
    Http.request
        { method = "POST"
        , headers =
            [ authHeader auth
            , Http.header "Dropbox-API-Arg" dropboxArg
            ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse parse
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.toTask
        |> Task.mapError decodeError


{-| Your intent when writing a file to some path.
See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>
-}
type WriteMode
    = Add
    | Overwrite
    | Update String


encodeWriteModel : WriteMode -> Json.Encode.Value
encodeWriteModel mode =
    case mode of
        Add ->
            Json.Encode.object [ ( ".tag", Json.Encode.string "add" ) ]

        Overwrite ->
            Json.Encode.object [ ( ".tag", Json.Encode.string "overwrite" ) ]

        Update rev ->
            Json.Encode.object
                [ ( ".tag", Json.Encode.string "update" )
                , ( "update", Json.Encode.string rev )
                ]


{-| Request parameters for `upload`
-}
type alias UploadRequest =
    { path : String
    , mode : WriteMode
    , autorename : Bool
    , clientModified : Maybe Date
    , mute : Bool
    , content : String
    }


{-| Additional information if the file is a photo or video.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>

-}
type MediaInfo
    = Pending
    | Metadata MediaMetadata


decodeMediaInfo : Json.Decode.Decoder MediaInfo
decodeMediaInfo =
    union
        [ tagVoid "pending" Pending
        , tagValue "metadata" Metadata decodeMediaMetadata
        ]


{-| Metadata for a photo or video.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>

-}
type MediaMetadata
    = Photo PhotoMetadata
    | Video VideoMetadata


decodeMediaMetadata : Json.Decode.Decoder MediaMetadata
decodeMediaMetadata =
    union
        [ tagObject "photo" Photo decodePhotoMetadata
        , tagObject "video" Video decodeVideoMetadata
        ]


{-| Metadata for a photo.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>

-}
type alias PhotoMetadata =
    { dimensions : Maybe Dimensions
    , location : Maybe GpsCoordinates
    , timeTaken : Maybe Date
    }


decodePhotoMetadata : Json.Decode.Decoder PhotoMetadata
decodePhotoMetadata =
    Pipeline.decode PhotoMetadata
        |> optional "dimensions" decodeDimensions
        |> optional "location" decodeGpsCoordinates
        |> optional "time_taken" Json.Decode.Extra.date


{-| Metadata for a video.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>

**WARNING**: elm-dropbox may give the incorrect values for `duration`,
since Elm currently does not provide a way to parse and represent 64-bit integers.

-}
type alias VideoMetadata =
    { dimensions : Maybe Dimensions
    , location : Maybe GpsCoordinates
    , timeTaken : Maybe Date
    , duration : Maybe Int -- XXX: should be UInt64
    }


decodeVideoMetadata : Json.Decode.Decoder VideoMetadata
decodeVideoMetadata =
    Pipeline.decode VideoMetadata
        |> optional "dimensions" decodeDimensions
        |> optional "location" decodeGpsCoordinates
        |> optional "time_taken" Json.Decode.Extra.date
        |> optional "duration" Json.Decode.int


{-| Dimensions for a photo or video.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>

**WARNING**: elm-dropbox may give the incorrect values,
since Elm currently does not provide a way to parse and represent 64-bit integers.

-}
type alias Dimensions =
    { height : Int -- XXX: should be UInt64
    , width : Int -- XXX: should be UInt64
    }


decodeDimensions : Json.Decode.Decoder Dimensions
decodeDimensions =
    Pipeline.decode Dimensions
        |> Pipeline.required "height" Json.Decode.int
        |> Pipeline.required "width" Json.Decode.int


{-| The GPS coordinate of the photo/video.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>

-}
type alias GpsCoordinates =
    { latitude : Float
    , longitude : Float
    }


decodeGpsCoordinates : Json.Decode.Decoder GpsCoordinates
decodeGpsCoordinates =
    Pipeline.decode GpsCoordinates
        |> Pipeline.required "latitude" Json.Decode.float
        |> Pipeline.required "longitude" Json.Decode.float


{-| Sharing info for a file which is contained by a shared folder.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>

-}
type alias FileSharingInfo =
    { readOnly : Bool
    , parentSharedFolderId : String
    , modifiedBy : Maybe String
    }


decodeFileSharingInfo : Json.Decode.Decoder FileSharingInfo
decodeFileSharingInfo =
    Pipeline.decode FileSharingInfo
        |> Pipeline.required "read_only" Json.Decode.bool
        |> Pipeline.required "parent_shared_folder_id" Json.Decode.string
        |> optional "modified_by" Json.Decode.string


{-| Collection of custom properties in filled property templates.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>

-}
type alias PropertyGroup =
    { templateId : String
    , fields : Dict String String
    }


decodePropertyGroup : Json.Decode.Decoder PropertyGroup
decodePropertyGroup =
    let
        decodeField =
            Json.Decode.map2 (,)
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.field "value" Json.Decode.string)
    in
    Pipeline.decode PropertyGroup
        |> Pipeline.required "template_id" Json.Decode.string
        |> Pipeline.required "fields" (Json.Decode.map Dict.fromList <| Json.Decode.list decodeField)


{-| Return value for `upload`

**WARNING**: elm-dropbox may give the incorrect values for `size`,
since Elm currently does not provide a way to parse and represent 64-bit integers.

-}
type alias UploadResponse =
    { name : String
    , id : String
    , clientModified : Date
    , serverModified : Date
    , rev : String
    , size : Int -- XXX: should be UInt64
    , pathLower : Maybe String
    , pathDisplay : Maybe String
    , parentSharedFolderId : Maybe String
    , mediaInfo : Maybe MediaInfo
    , sharingInfo : Maybe FileSharingInfo
    , propertyGroups : Maybe (List PropertyGroup)
    , hasExplicitSharedMembers : Maybe Bool
    , contentHash : Maybe String
    }


decodeUploadResponse : Json.Decode.Decoder UploadResponse
decodeUploadResponse =
    Pipeline.decode UploadResponse
        |> Pipeline.required "name" Json.Decode.string
        |> Pipeline.required "id" Json.Decode.string
        |> Pipeline.required "client_modified" Json.Decode.Extra.date
        |> Pipeline.required "server_modified" Json.Decode.Extra.date
        |> Pipeline.required "rev" Json.Decode.string
        |> Pipeline.required "size" Json.Decode.int
        |> optional "path_lower" Json.Decode.string
        |> optional "path_display" Json.Decode.string
        |> optional "parent_shared_folder_id" Json.Decode.string
        |> optional "media_info" decodeMediaInfo
        |> optional "sharing_info" decodeFileSharingInfo
        |> optional "property_groups" (Json.Decode.list decodePropertyGroup)
        |> optional "has_explicit_shared_members" Json.Decode.bool
        |> optional "content_hash" Json.Decode.string


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>
-}
type UploadError
    = Path UploadWriteFailed
    | OtherUploadError String Json.Encode.Value
    | OtherUploadFailure Http.Error


decodeUploadError : Json.Decode.Decoder UploadError
decodeUploadError =
    Json.Decode.field "error" <|
        openUnion OtherUploadError
            [ tagObject "path" Path decodeUploadWriteFailed
            ]


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>
-}
type alias UploadWriteFailed =
    { reason : WriteError
    , uploadSessionId : String
    }


decodeUploadWriteFailed : Json.Decode.Decoder UploadWriteFailed
decodeUploadWriteFailed =
    Pipeline.decode UploadWriteFailed
        |> Pipeline.required "reason" decodeWriteError
        |> Pipeline.required "upload_session_id" Json.Decode.string


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>
-}
type WriteError
    = MalformedPathWrite (Maybe String)
    | Conflict WriteConflictError
    | NoWritePermission
    | InsufficientSpace
    | DisallowedName
    | TeamFolder
    | OtherWriteError String Json.Encode.Value


decodeWriteError : Json.Decode.Decoder WriteError
decodeWriteError =
    openUnion OtherWriteError
        [ tagValue "malformed_path" MalformedPathWrite (Json.Decode.nullable Json.Decode.string)
        , tagValue "conflict" Conflict decodeWriteConflictError
        , tagVoid "no_write_permission" NoWritePermission
        , tagVoid "insufficient_space" InsufficientSpace
        , tagVoid "disallowed_name" DisallowedName
        , tagVoid "team_folder" TeamFolder
        ]


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>
-}
type WriteConflictError
    = File
    | Folder
    | FileAncestor
    | OtherWriteConflictError String Json.Encode.Value


decodeWriteConflictError : Json.Decode.Decoder WriteConflictError
decodeWriteConflictError =
    openUnion OtherWriteConflictError
        [ tagVoid "file" File
        , tagVoid "folder" Folder
        , tagVoid "file_ancestor" FileAncestor
        ]


{-| Create a new file with the contents provided in the request.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>

-}
upload : UserAuth -> UploadRequest -> Task UploadError UploadResponse
upload auth info =
    let
        url =
            "https://content.dropboxapi.com/2/files/upload"

        body =
            Http.stringBody "application/octet-stream" info.content

        dropboxArg =
            Json.Encode.encode 0 <|
                Json.Encode.object <|
                    List.filterMap identity
                        [ Just ( "path", Json.Encode.string info.path )
                        , Just ( "mode", encodeWriteModel info.mode )
                        , Just ( "autorename", Json.Encode.bool info.autorename )
                        , info.clientModified
                            |> Maybe.map Date.Format.formatISO8601
                            |> Maybe.map Json.Encode.string
                            |> Maybe.map ((,) "client_modified")
                        , Just ( "mute", Json.Encode.bool info.mute )
                        ]

        decodeError err =
            case err of
                Http.BadStatus response ->
                    case Json.Decode.decodeString decodeUploadError response.body of
                        Ok err ->
                            err

                        Err _ ->
                            OtherUploadFailure err

                _ ->
                    OtherUploadFailure err
    in
    Http.request
        { method = "POST"
        , headers =
            [ authHeader auth
            , Http.header "Dropbox-API-Arg" dropboxArg
            ]
        , url = url
        , body = body
        , expect = Http.expectJson decodeUploadResponse
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.toTask
        |> Task.mapError decodeError


{-| This provides the simplest way to integrate Dropbox authentication.
Using `Dropbox.program` will handle parsing the authentication response from the
authentication redirect so that you don't have to do it manually.
-}
program :
    { init : Navigation.Location -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    , onAuth : Result String UserAuth -> msg
    }
    -> Program Never model (Maybe msg)
program config =
    Navigation.program (always Nothing)
        { init =
            \location ->
                case parseAuth location of
                    Nothing ->
                        config.init location
                            |> Update.Extra.mapCmd Just

                    Just response ->
                        config.init location
                            |> Update.Extra.andThen
                                config.update
                                (config.onAuth <| authorization <| response)
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
