module Dropbox exposing
    ( application, Msg
    , AuthorizeRequest, Role(..), authorize, AuthorizeResult(..), AuthorizeError, UserAuth, encodeUserAuth, decodeUserAuth
    , authorizationUrl, redirectUriFromLocation, authorizationFromAccessToken, parseAuthorizeResult
    , tokenRevoke
    , Metadata(..), FileMetadata, FolderMetadata, DeletedMetadata
    , download, DownloadRequest, DownloadResponse, DownloadError(..), LookupError(..)
    , upload, UploadRequest, WriteMode(..), UploadError(..), UploadWriteFailed, WriteError(..)
    , listFolder, listFolderContinue, ListFolderRequest, ListFolderResponse, ListFolderError(..), ListFolderContinueError(..)
    , MediaInfo, MediaMetadata, PhotoMetadata, VideoMetadata, Dimensions, GpsCoordinates, FileSharingInfo, PropertyGroup
    )

{-|


## Dropbox API

See the official Dropbox documentation at
<https://www.dropbox.com/developers/documentation/http/documentation>

@docs application, Msg


### Authorization

@docs AuthorizeRequest, Role, authorize, AuthorizeResult, AuthorizeError, UserAuth, encodeUserAuth, decodeUserAuth
@docs authorizationUrl, redirectUriFromLocation, authorizationFromAccessToken, parseAuthorizeResult


### Auth

@docs tokenRevoke


### Files

@docs Metadata, FileMetadata, FolderMetadata, DeletedMetadata
@docs download, DownloadRequest, DownloadResponse, DownloadError, LookupError
@docs upload, UploadRequest, WriteMode, UploadError, UploadWriteFailed, WriteError
@docs listFolder, listFolderContinue, ListFolderRequest, ListFolderResponse, ListFolderError, ListFolderContinueError

@docs MediaInfo, MediaMetadata, PhotoMetadata, VideoMetadata, Dimensions, GpsCoordinates, FileSharingInfo, PropertyGroup

-}

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html)
import Http
import Iso8601
import Json.Decode
import Json.Decode.Dropbox exposing (openUnion, optional, tagObject, tagValue, tagVoid, union)
import Json.Decode.Extra
import Json.Decode.Pipeline as Pipeline
import Json.Encode
import Task exposing (Task)
import Time
import Url exposing (Url)


{-| Request parameters for Dropbox OAuth 2.0 authorization requests.

See <https://www.dropbox.com/developers/documentation/http/documentation#oauth2-authorize>

Note: `redirect_uri` is not present here because it is provided directly to
`Dropbox.authorize` or `Dropbox.authorizationUrl`.

-}
type alias AuthorizeRequest =
    { clientId : String
    , state : Maybe String
    , requireRole : Maybe Role
    , forceReapprove : Bool
    , disableSignup : Bool
    , locale : Maybe String
    , forceReauthentication : Bool
    }


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#oauth2-authorize>
-}
type Role
    = Personal
    | Work


{-| Return value of the `authorize` endpoint, which is the data Dropbox returns via
the redirect URL.

You can get the `AuthorizeResult` by using `Dropbox.program`,
or by using `parseAuthorizeResult` if you need to manually parse the redirect URL.

See <https://www.dropbox.com/developers/documentation/http/documentation#oauth2-authorize>

Note: `uid` is not provided because it is deprecated.
See <https://www.dropbox.com/developers/documentation/http/documentation#oauth2-authorize>

-}
type AuthorizeResult
    = DropboxAuthorizeErr AuthorizeError
    | AuthorizeOk
        { userAuth : UserAuth
        , accountId : String
        , state : Maybe String
        }
    | UnknownAccessTokenErr
        { accessToken : String
        , tokenType : String
        , accountId : String
        , state : Maybe String
        }


{-| Return value of the `authorize` endpoint when authentication fails. See `AuthorizeResult`.

See <https://www.dropbox.com/developers/documentation/http/documentation#oauth2-authorize>

-}
type alias AuthorizeError =
    { error : String
    , errorDescription : String
    , state : Maybe String
    }


{-| The Dropbox OAuth 2.0 authorization URL.
Typically you will just want to use `authorize` instead,
which will initiate the authorization.

See <https://www.dropbox.com/developers/reference/oauth-guide>

-}
authorizationUrl : AuthorizeRequest -> String -> String
authorizationUrl request redirectUri =
    -- TODO: ensure that state and redirect_uri are properly escaped
    let
        param k v =
            k ++ "=" ++ v

        roleToString role =
            case role of
                Personal ->
                    "personal"

                Work ->
                    "work"

        boolToString bool =
            case bool of
                True ->
                    "true"

                False ->
                    "false"
    in
    String.concat
        [ "https://www.dropbox.com/oauth2/authorize"
        , "?"
        , String.join "&" <|
            List.filterMap identity
                [ Just <| param "response_type" "token"
                , Just <| param "client_id" request.clientId
                , Just <| param "redirect_uri" redirectUri
                , Maybe.map (param "state") request.state
                , Maybe.map (param "require_role" << roleToString) request.requireRole
                , Just <| param "force_reapprove" <| boolToString request.forceReapprove
                , Just <| param "disable_signup" <| boolToString request.disableSignup
                , Maybe.map (param "locale") request.locale
                , Just <| param "force_reauthentication" <| boolToString request.forceReauthentication
                ]
        ]


{-| Generate a redirect URI from a `Url`.

Typically you will want to use `Dropbox.authorize`, which will do this automatically.
You may want to use this if you need to manually manage the OAuth flow.

-}
redirectUriFromLocation : Url -> String
redirectUriFromLocation location =
    let
        protocol =
            case location.protocol of
                Url.Http ->
                    "http:"

                Url.Https ->
                    "https:"

        port_ =
            case location.port_ of
                Nothing ->
                    ""

                Just p ->
                    ":" ++ String.fromInt p
    in
    protocol
        ++ "//"
        ++ location.host
        ++ port_
        ++ location.path


{-| <https://www.dropbox.com/developers/documentation/http/documentation#oauth2-authorize>
-}
authorize : AuthorizeRequest -> Url -> Cmd msg
authorize request location =
    Browser.Navigation.load <|
        authorizationUrl request (redirectUriFromLocation location)


{-| Read an `AuthorizeResult` from the page location.

Typically you will want to use [`Dropbox.program`](#program) instead, which will do this automatically.
You may want to use this if you need to manually manage the OAuth flow.

-}
parseAuthorizeResult : Url -> Maybe AuthorizeResult
parseAuthorizeResult location =
    let
        isKeyValue list =
            case list of
                [ k, v ] ->
                    Just ( k, v )

                _ ->
                    Nothing

        params hash =
            hash
                |> String.split "&"
                |> List.map (String.split "=")
                |> List.filterMap isKeyValue
                |> Dict.fromList

        makeAuth dict =
            Maybe.map3 (makeSuccess dict)
                (Dict.get "access_token" dict)
                (Dict.get "token_type" dict)
                (Dict.get "account_id" dict)

        makeSuccess dict accessToken tokenType accountId =
            -- TODO: handle unescaping of state, account_id
            case authorization tokenType accessToken of
                Nothing ->
                    UnknownAccessTokenErr
                        { accessToken = accessToken
                        , tokenType = tokenType
                        , accountId = accountId
                        , state = Dict.get "state" dict
                        }

                Just auth ->
                    AuthorizeOk
                        { userAuth = auth
                        , accountId = accountId
                        , state = Dict.get "state" dict
                        }

        makeError dict =
            Maybe.map2 AuthorizeError
                (Dict.get "error" dict)
                (Dict.get "error_description" dict)
                |> Maybe.map
                    (\partial ->
                        partial
                            -- TODO: handle unescaping of state, error_description
                            (Dict.get "state" dict)
                    )
                |> Maybe.map DropboxAuthorizeErr

        orElseLazy other value =
            case value of
                Nothing ->
                    other ()

                Just x ->
                    Just x
    in
    case location.fragment of
        Just hash ->
            makeAuth (params hash)
                |> orElseLazy (\() -> makeError (params hash))

        Nothing ->
            Nothing


{-| A user authentication token that can be used to authenticate API calls

See <https://www.dropbox.com/developers/reference/auth-types#user>

-}
type UserAuth
    = Bearer String


{-| Encode a `UserAuth` to JSON.

You should consider the resulting value to be opaque
and only read it using `decodeUserAuth`.

WARNING: To protect your users' security,
you must not transmit the resulting value off the user's device.
This function exists to allow persisting the auth token to localStorage
or other storage that is local to the user's device _and_ private to your application.
You should not send this value to your own server
(if you think you need that, you should use a different OAuth flow
involving your Dropox app's app secret instead of using implicit grant).

-}
encodeUserAuth : UserAuth -> Json.Encode.Value
encodeUserAuth auth =
    case auth of
        Bearer authToken ->
            Json.Encode.object
                [ ( "dropbox", Json.Encode.string authToken )
                ]


{-| Decode a `UserAuth` encoded with `encodeUserAuth`.

NOTE: See the security warning in `encodeUserAuth`.

If you have an auth token as a String and need to convert it to a `UserAuth`,
see [`authorizationFromAccessToken`](#authorizationFromAccessToken).

-}
decodeUserAuth : Json.Decode.Decoder UserAuth
decodeUserAuth =
    Json.Decode.field "dropbox" Json.Decode.string
        |> Json.Decode.map Bearer


authorization : String -> String -> Maybe UserAuth
authorization tokenType accessToken =
    case tokenType of
        "bearer" ->
            Just <| Bearer accessToken

        _ ->
            Nothing


{-| Create a `UserAuth` from a Dropbox access token.

You can use this during development, using the "generated access token" from the settings page of [your Dropbox app](https://www.dropbox.com/developers/apps).

You should not use this in a production app.
Instead, you should use the normal authorization flow and use [`program`](#program) or [`parseAuthorizeResult`](#parseAuthorizeResult).

-}
authorizationFromAccessToken : String -> UserAuth
authorizationFromAccessToken accessToken =
    Bearer accessToken


authHeader : UserAuth -> Http.Header
authHeader auth =
    case auth of
        Bearer accessToken ->
            Http.header "Authorization" ("Bearer " ++ accessToken)


{-| Disables the access token used to authenticate the call.

See <https://www.dropbox.com/developers/documentation/http/documentation#auth-token-revoke>

-}
tokenRevoke : UserAuth -> Task Http.Error ()
tokenRevoke auth =
    let
        url =
            "https://api.dropboxapi.com/2/auth/token/revoke"

        resolver =
            Http.stringResolver <|
                \response ->
                    case response of
                        Http.GoodStatus_ metadata body ->
                            Ok ()

                        Http.BadStatus_ metadata body ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.BadUrl_ message ->
                            Err (Http.BadUrl message)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError
    in
    Http.task
        { method = "POST"
        , headers =
            [ authHeader auth
            ]
        , url = url
        , body = Http.emptyBody
        , resolver = resolver
        , timeout = Nothing
        }


decodeDate : Json.Decode.Decoder Time.Posix
decodeDate =
    let
        fromResult result =
            case result of
                Err _ ->
                    Json.Decode.fail "Not a valid date"

                Ok value ->
                    Json.Decode.succeed value
    in
    Json.Decode.string
        |> Json.Decode.map Iso8601.toTime
        |> Json.Decode.andThen fromResult


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
    , clientModified : Time.Posix
    , serverModified : Time.Posix
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
    Json.Decode.succeed DownloadResponse
        |> Pipeline.hardcoded content
        |> Pipeline.required "name" Json.Decode.string
        |> Pipeline.required "id" Json.Decode.string
        |> Pipeline.required "client_modified" decodeDate
        |> Pipeline.required "server_modified" decodeDate
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


{-| The resolver decodes a JSON,
and in the case of a bad status code, can decode an error value
-}
jsonBodyAndErrorResolver : Json.Decode.Decoder a -> Json.Decode.Decoder x -> (Http.Error -> x) -> Http.Resolver x a
jsonBodyAndErrorResolver decoder errorDecoder httpError =
    Http.stringResolver <|
        \response ->
            case response of
                Http.GoodStatus_ metadata body ->
                    Json.Decode.decodeString decoder body
                        |> Result.mapError (Json.Decode.errorToString >> Http.BadBody >> httpError)

                Http.BadStatus_ metadata body ->
                    case Json.Decode.decodeString errorDecoder body of
                        Ok errValue ->
                            Err errValue

                        Err _ ->
                            Err (httpError (Http.BadStatus metadata.statusCode))

                Http.BadUrl_ message ->
                    Err (httpError (Http.BadUrl message))

                Http.Timeout_ ->
                    Err (httpError Http.Timeout)

                Http.NetworkError_ ->
                    Err (httpError Http.NetworkError)


{-| The resolver decodes response data from the `dropbox-api-result` header,
and in the case of a bad status code, can decode an error value
-}
dropboxApiResultResolver : (String -> Json.Decode.Decoder a) -> Json.Decode.Decoder x -> (Http.Error -> x) -> Http.Resolver x a
dropboxApiResultResolver decoder errorDecoder httpError =
    Http.stringResolver <|
        \response ->
            case response of
                Http.GoodStatus_ metadata body ->
                    case Dict.get "dropbox-api-result" metadata.headers of
                        Nothing ->
                            Err (httpError (Http.BadBody "No dropbox-api-result header found"))

                        Just arg ->
                            Json.Decode.decodeString (decoder body) arg
                                |> Result.mapError (Json.Decode.errorToString >> Http.BadBody >> httpError)

                Http.BadStatus_ metadata body ->
                    case Json.Decode.decodeString errorDecoder body of
                        Ok errValue ->
                            Err errValue

                        Err _ ->
                            Err (httpError (Http.BadStatus metadata.statusCode))

                Http.BadUrl_ message ->
                    Err (httpError (Http.BadUrl message))

                Http.Timeout_ ->
                    Err (httpError Http.Timeout)

                Http.NetworkError_ ->
                    Err (httpError Http.NetworkError)


{-| Download a file from a user's Dropbox.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-download>

-}
download : UserAuth -> DownloadRequest -> Task DownloadError DownloadResponse
download auth info =
    let
        url =
            "https://content.dropboxapi.com/2/files/download"

        dropboxArg =
            Json.Encode.encode 0 <|
                Json.Encode.object
                    [ ( "path", Json.Encode.string info.path ) ]
    in
    Http.task
        { method = "POST"
        , headers =
            [ authHeader auth
            , Http.header "Dropbox-API-Arg" dropboxArg
            ]
        , url = url
        , body = Http.emptyBody
        , resolver = dropboxApiResultResolver decodeDownloadResponse decodeDownloadError OtherDownloadFailure
        , timeout = Nothing
        }


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
    , clientModified : Maybe Time.Posix
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
    , timeTaken : Maybe Time.Posix
    }


decodePhotoMetadata : Json.Decode.Decoder PhotoMetadata
decodePhotoMetadata =
    Json.Decode.succeed PhotoMetadata
        |> optional "dimensions" decodeDimensions
        |> optional "location" decodeGpsCoordinates
        |> optional "time_taken" decodeDate


{-| Metadata for a video.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-upload>

**WARNING**: elm-dropbox may give the incorrect values for `duration`,
since Elm currently does not provide a way to parse and represent 64-bit integers.

-}
type alias VideoMetadata =
    { dimensions : Maybe Dimensions
    , location : Maybe GpsCoordinates
    , timeTaken : Maybe Time.Posix
    , duration : Maybe Int -- XXX: should be UInt64
    }


decodeVideoMetadata : Json.Decode.Decoder VideoMetadata
decodeVideoMetadata =
    Json.Decode.succeed VideoMetadata
        |> optional "dimensions" decodeDimensions
        |> optional "location" decodeGpsCoordinates
        |> optional "time_taken" decodeDate
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
    Json.Decode.succeed Dimensions
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
    Json.Decode.succeed GpsCoordinates
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
    Json.Decode.succeed FileSharingInfo
        |> Pipeline.required "read_only" Json.Decode.bool
        |> Pipeline.required "parent_shared_folder_id" Json.Decode.string
        |> optional "modified_by" Json.Decode.string


{-| Sharing info for a folder which is contained by a shared folder.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder>

-}
type alias FolderSharingInfo =
    { readOnly : Bool
    , parentSharedFolderId : Maybe String
    , sharedFolderId : Maybe String
    , traverseOnly : Bool
    , noAccess : Bool
    }


decodeFolderSharingInfo : Json.Decode.Decoder FolderSharingInfo
decodeFolderSharingInfo =
    Json.Decode.succeed FolderSharingInfo
        |> Pipeline.required "read_only" Json.Decode.bool
        |> optional "parent_shared_folder_id" Json.Decode.string
        |> optional "shared_folder_id" Json.Decode.string
        |> Pipeline.optional "traverse_only" Json.Decode.bool False
        |> Pipeline.optional "no_access" Json.Decode.bool False


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
            Json.Decode.map2 (\a b -> ( a, b ))
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.field "value" Json.Decode.string)
    in
    Json.Decode.succeed PropertyGroup
        |> Pipeline.required "template_id" Json.Decode.string
        |> Pipeline.required "fields" (Json.Decode.map Dict.fromList <| Json.Decode.list decodeField)


{-| File metadata

**WARNING**: elm-dropbox may give the incorrect values for `size`,
since Elm currently does not provide a way to parse and represent 64-bit integers.

-}
type alias FileMetadata =
    { name : String
    , id : String
    , clientModified : Time.Posix
    , serverModified : Time.Posix
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


{-| Folder metadata
-}
type alias FolderMetadata =
    { name : String
    , id : String
    , pathLower : Maybe String
    , pathDisplay : Maybe String
    , parentSharedFolderId : Maybe String
    , sharedFolderId : Maybe String
    , sharingInfo : Maybe FolderSharingInfo
    , propertyGroups : Maybe (List PropertyGroup)
    }


{-| Deleted item metadata
-}
type alias DeletedMetadata =
    { name : String
    , pathLower : Maybe String
    , pathDisplay : Maybe String
    , parentSharedFolderId : Maybe String
    }


{-| Metadata union type
-}
type Metadata
    = FileMeta FileMetadata
    | FolderMeta FolderMetadata
    | DeletedMeta DeletedMetadata


decodeFileMetadata : Json.Decode.Decoder FileMetadata
decodeFileMetadata =
    Json.Decode.succeed FileMetadata
        |> Pipeline.required "name" Json.Decode.string
        |> Pipeline.required "id" Json.Decode.string
        |> Pipeline.required "client_modified" decodeDate
        |> Pipeline.required "server_modified" decodeDate
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


decodeFolderMetadata : Json.Decode.Decoder FolderMetadata
decodeFolderMetadata =
    Json.Decode.succeed FolderMetadata
        |> Pipeline.required "name" Json.Decode.string
        |> Pipeline.required "id" Json.Decode.string
        |> optional "path_lower" Json.Decode.string
        |> optional "path_display" Json.Decode.string
        |> optional "parent_shared_folder_id" Json.Decode.string
        |> optional "shared_folder_id" Json.Decode.string
        |> optional "sharing_info" decodeFolderSharingInfo
        |> optional "property_groups" (Json.Decode.list decodePropertyGroup)


decodeDeletedMetadata : Json.Decode.Decoder DeletedMetadata
decodeDeletedMetadata =
    Json.Decode.succeed DeletedMetadata
        |> Pipeline.required "name" Json.Decode.string
        |> optional "path_lower" Json.Decode.string
        |> optional "path_display" Json.Decode.string
        |> optional "parent_shared_folder_id" Json.Decode.string


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
    Json.Decode.succeed UploadWriteFailed
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
upload : UserAuth -> UploadRequest -> Task UploadError FileMetadata
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
                            |> Maybe.map Iso8601.fromTime
                            |> Maybe.map Json.Encode.string
                            |> Maybe.map (\b -> ( "client_modified", b ))
                        , Just ( "mute", Json.Encode.bool info.mute )
                        ]
    in
    Http.task
        { method = "POST"
        , headers =
            [ authHeader auth
            , Http.header "Dropbox-API-Arg" dropboxArg
            ]
        , url = url
        , body = body
        , resolver = jsonBodyAndErrorResolver decodeFileMetadata decodeUploadError OtherUploadFailure
        , timeout = Nothing
        }


{-| Request parameters for `listFolder`
-}
type alias ListFolderRequest =
    { path : String
    , recursive : Bool
    , includeMediaInfo : Bool
    , includeDeleted : Bool
    , includeHasExplicitSharedMembers : Bool
    }


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder>
-}
type ListFolderError
    = PathListError LookupError
    | OtherListError String Json.Encode.Value
    | OtherListFailure Http.Error


decodeListError : Json.Decode.Decoder ListFolderError
decodeListError =
    Json.Decode.field "error" <|
        openUnion OtherListError
            [ tagValue "path" PathListError decodeLookupError
            ]


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder>
-}
type alias ListFolderResponse =
    { entries : List Metadata
    , cursor : String
    , hasMore : Bool
    }


decodeListResponse : Json.Decode.Decoder ListFolderResponse
decodeListResponse =
    Json.Decode.succeed ListFolderResponse
        |> Pipeline.required "entries"
            (Json.Decode.list
                (union
                    [ tagObject "file" FileMeta decodeFileMetadata
                    , tagObject "folder" FolderMeta decodeFolderMetadata
                    , tagObject "deleted" DeletedMeta decodeDeletedMetadata
                    ]
                )
            )
        |> Pipeline.required "cursor" Json.Decode.string
        |> Pipeline.required "has_more" Json.Decode.bool


{-| Begin listing the contents of a folder.

See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder>

-}
listFolder : UserAuth -> ListFolderRequest -> Task ListFolderError ListFolderResponse
listFolder auth options =
    let
        url =
            "https://api.dropboxapi.com/2/files/list_folder"

        body =
            Json.Encode.encode 0 <|
                Json.Encode.object <|
                    [ ( "path", Json.Encode.string options.path )
                    , ( "recursive", Json.Encode.bool options.recursive )
                    , ( "include_media_info", Json.Encode.bool options.includeMediaInfo )
                    , ( "include_deleted", Json.Encode.bool options.includeDeleted )
                    , ( "include_has_explicit_shared_members", Json.Encode.bool options.includeHasExplicitSharedMembers )
                    ]
    in
    Http.task
        { method = "POST"
        , headers =
            [ authHeader auth ]
        , url = url
        , body = Http.stringBody "application/json" body
        , resolver = jsonBodyAndErrorResolver decodeListResponse decodeListError OtherListFailure
        , timeout = Nothing
        }


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder-continue>
-}
type ListFolderContinueError
    = PathListContinueError LookupError
    | ExpiredCursorError
    | OtherListContinueError String Json.Encode.Value
    | OtherListContinueFailure Http.Error


decodeListContinueError : Json.Decode.Decoder ListFolderContinueError
decodeListContinueError =
    Json.Decode.field "error" <|
        openUnion OtherListContinueError
            [ tagValue "path" PathListContinueError decodeLookupError
            , tagVoid "reset" ExpiredCursorError
            ]


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder-continue>
-}
type alias ListFolderContinueRequest =
    { cursor : String
    }


{-| See <https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder-continue>
-}
listFolderContinue : UserAuth -> ListFolderContinueRequest -> Task ListFolderContinueError ListFolderResponse
listFolderContinue auth cursorInfo =
    let
        url =
            "https://api.dropboxapi.com/2/files/list_folder/continue"

        body =
            Json.Encode.encode 0 <|
                Json.Encode.object <|
                    [ ( "cursor", Json.Encode.string cursorInfo.cursor )
                    ]
    in
    Http.task
        { method = "POST"
        , headers =
            [ authHeader auth ]
        , url = url
        , body = Http.stringBody "application/json" body
        , resolver = jsonBodyAndErrorResolver decodeListResponse decodeListContinueError OtherListContinueFailure
        , timeout = Nothing
        }


{-| The message type for an app that uses `Dropbox.program`
-}
type Msg msg
    = Msg (Maybe msg)


{-| This provides the simplest way to integrate Dropbox authentication.
Using `Dropbox.application` will handle parsing the authentication response from the
authentication redirect so that you don't have to do it manually.
-}
application :
    { init : flags -> Url -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Browser.Document msg
    , onAuth : AuthorizeResult -> msg
    }
    -> Program flags model (Msg msg)
application config =
    let
        andThen : (msg -> model -> ( model, Cmd a )) -> msg -> ( model, Cmd a ) -> ( model, Cmd a )
        andThen update msg ( model, cmd ) =
            let
                ( model_, cmd_ ) =
                    update msg model
            in
            ( model_, Cmd.batch [ cmd, cmd_ ] )
    in
    Browser.application
        { init =
            \flags url key ->
                case parseAuthorizeResult url of
                    Nothing ->
                        config.init flags url
                            |> Tuple.mapSecond (Cmd.map (Msg << Just))

                    Just response ->
                        config.init flags url
                            |> andThen
                                config.update
                                (config.onAuth response)
                            |> Tuple.mapSecond (Cmd.map (Msg << Just))
        , update =
            \(Msg msg) model ->
                case msg of
                    Nothing ->
                        ( model, Cmd.none )

                    Just m ->
                        config.update m model
                            |> Tuple.mapSecond (Cmd.map (Msg << Just))
        , subscriptions = config.subscriptions >> Sub.map (Msg << Just)
        , view = config.view >> mapDocument (Msg << Just)
        , onUrlRequest = always <| Msg Nothing
        , onUrlChange = always <| Msg Nothing
        }


mapDocument : (a -> b) -> Browser.Document a -> Browser.Document b
mapDocument f document =
    { title = document.title
    , body = List.map (Html.map f) document.body
    }
