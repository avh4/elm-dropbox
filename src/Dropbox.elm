module Dropbox
    exposing
        ( AuthorizeError
        , AuthorizeRequest
        , AuthorizeResult(..)
        , DeletedMetadata
        , Dimensions
        , DownloadError(..)
        , DownloadRequest
        , DownloadResponse
        , FileMetadata
        , FileSharingInfo
        , FolderMetadata
        , GpsCoordinates
        , ListFolderContinueError(..)
        , ListFolderError(..)
        , ListFolderRequest
        , ListFolderResponse
        , LookupError(..)
        , MediaInfo
        , MediaMetadata
        , Metadata(..)
        , Msg
        , PhotoMetadata
        , PropertyGroup
        , Role(..)
        , UploadError(..)
        , UploadRequest
        , UploadWriteFailed
        , UserAuth
        , VideoMetadata
        , WriteError(..)
        , WriteMode(..)
        , authorizationFromAccessToken
        , authorizationUrl
        , authorize
        , decodeUserAuth
        , download
        , encodeUserAuth
        , listFolder
        , listFolderContinue
        , parseAuthorizeResult
        , program
        , programWithFlags
        , redirectUriFromLocation
        , tokenRevoke
        , upload
        )

{-|


## Dropbox API

See the official Dropbox documentation at
<https://www.dropbox.com/developers/documentation/http/documentation>

@docs program, programWithFlags, Msg


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
import Maybe.Extra
import Navigation
import Task exposing (Task)
import Update.Extra


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


{-| Generate a redirect URI from a `Navigation.Location`.

Typically you will want to use `Dropbox.authorize`, which will do this automatically.
You may want to use this if you need to manually manage the OAuth flow.

-}
redirectUriFromLocation : Navigation.Location -> String
redirectUriFromLocation location =
    location.protocol
        ++ "//"
        ++ location.host
        ++ location.pathname


{-| <https://www.dropbox.com/developers/documentation/http/documentation#oauth2-authorize>
-}
authorize : AuthorizeRequest -> Navigation.Location -> Cmd msg
authorize request location =
    Navigation.load <|
        authorizationUrl request (redirectUriFromLocation location)


{-| Read an `AuthorizeResult` from the page location.

Typically you will want to use [`Dropbox.program`](#program) instead, which will do this automatically.
You may want to use this if you need to manually manage the OAuth flow.

-}
parseAuthorizeResult : Navigation.Location -> Maybe AuthorizeResult
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
    in
        case String.uncons location.hash of
            Just ( '#', hash ) ->
                makeAuth (params hash)
                    |> Maybe.Extra.orElseLazy (\() -> makeError (params hash))

            _ ->
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
or other storage that is local to the user's device *and* private to your application.
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
    Pipeline.decode FolderSharingInfo
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
            Json.Decode.map2 (,)
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.field "value" Json.Decode.string)
    in
        Pipeline.decode PropertyGroup
            |> Pipeline.required "template_id" Json.Decode.string
            |> Pipeline.required "fields" (Json.Decode.map Dict.fromList <| Json.Decode.list decodeField)


{-| File metadata

**WARNING**: elm-dropbox may give the incorrect values for `size`,
since Elm currently does not provide a way to parse and represent 64-bit integers.

-}
type alias FileMetadata =
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
    Pipeline.decode FileMetadata
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


decodeFolderMetadata : Json.Decode.Decoder FolderMetadata
decodeFolderMetadata =
    Pipeline.decode FolderMetadata
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
    Pipeline.decode DeletedMetadata
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
            , expect = Http.expectJson decodeFileMetadata
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.toTask
            |> Task.mapError decodeError


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
    Pipeline.decode ListFolderResponse
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

        decodeError err =
            case err of
                Http.BadStatus response ->
                    case Json.Decode.decodeString decodeListError response.body of
                        Ok err ->
                            err

                        Err _ ->
                            OtherListFailure err

                _ ->
                    OtherListFailure err
    in
        Http.request
            { method = "POST"
            , headers =
                [ authHeader auth ]
            , url = url
            , body = Http.stringBody "application/json" body
            , expect = Http.expectJson decodeListResponse
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.toTask
            |> Task.mapError decodeError


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

        decodeError err =
            case err of
                Http.BadStatus response ->
                    case Json.Decode.decodeString decodeListContinueError response.body of
                        Ok err ->
                            err

                        Err _ ->
                            OtherListContinueFailure err

                _ ->
                    OtherListContinueFailure err
    in
        Http.request
            { method = "POST"
            , headers =
                [ authHeader auth ]
            , url = url
            , body = Http.stringBody "application/json" body
            , expect = Http.expectJson decodeListResponse
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.toTask
            |> Task.mapError decodeError


{-| The message type for an app that uses `Dropbox.program`
-}
type Msg msg
    = Msg (Maybe msg)


{-| This provides the simplest way to integrate Dropbox authentication.
Using `Dropbox.program` will handle parsing the authentication response from the
authentication redirect so that you don't have to do it manually.
-}
program :
    { init : Navigation.Location -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    , onAuth : AuthorizeResult -> msg
    }
    -> Program Never model (Msg msg)
program config =
    let
        configWithFlags =
            navigationConfig <|
                { config | init = \() -> config.init }
    in
        Navigation.program (always <| Msg Nothing) <|
            { configWithFlags | init = configWithFlags.init () }


{-| This provides the simplest way to integrate Dropbox authentication with
flags. Using `Dropbox.programWithFlags` will handle parsing the authentication
response from the authentication redirect so that you don't have to do it
manually.
-}
programWithFlags :
    { init : flags -> Navigation.Location -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    , onAuth : AuthorizeResult -> msg
    }
    -> Program flags model (Msg msg)
programWithFlags config =
    Navigation.programWithFlags (always <| Msg Nothing) <|
        navigationConfig config


type alias NavigationConfig flags model msg =
    { init : flags -> Navigation.Location -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


navigationConfig :
    { init : flags -> Navigation.Location -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    , onAuth : AuthorizeResult -> msg
    }
    -> NavigationConfig flags model (Msg msg)
navigationConfig config =
    { init =
        \flags location ->
            case parseAuthorizeResult location of
                Nothing ->
                    config.init flags location
                        |> Update.Extra.mapCmd (Msg << Just)

                Just response ->
                    config.init flags location
                        |> Update.Extra.andThen
                            config.update
                            (config.onAuth response)
                        |> Update.Extra.mapCmd (Msg << Just)
    , update =
        \(Msg msg) model ->
            case msg of
                Nothing ->
                    ( model, Cmd.none )

                Just m ->
                    config.update m model
                        |> Update.Extra.mapCmd (Msg << Just)
    , subscriptions = config.subscriptions >> Sub.map (Msg << Just)
    , view = config.view >> Html.map (Msg << Just)
    }
