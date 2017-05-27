module Json.Decode.Dropbox exposing (openUnion, optional, tagObject, tagValue, tagVoid, union)

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Decode.Pipeline as Pipeline


optional : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
optional field decoder =
    Pipeline.optional field (nullable decoder) Nothing


tagVoid : String -> a -> ( String, Decoder a )
tagVoid tagName elmTag =
    ( tagName, succeed elmTag )


tagValue : String -> (inner -> a) -> Decoder inner -> ( String, Decoder a )
tagValue tagName elmTag decoder =
    ( tagName, field tagName <| map elmTag decoder )


tagObject : String -> (inner -> a) -> Decoder inner -> ( String, Decoder a )
tagObject tagName elmTag decoder =
    ( tagName, map elmTag decoder )


openUnion : (String -> Value -> a) -> List ( String, Decoder a ) -> Decoder a
openUnion other tags =
    decodeOpenUnion ".tag"
        tags
        (\type_ -> map (other type_) value)


union : List ( String, Decoder a ) -> Decoder a
union =
    decodeUnion ".tag"



-- TODO: contribute to elm-community/json-extra


decodeOpenUnion : String -> List ( String, Json.Decode.Decoder a ) -> (String -> Json.Decode.Decoder a) -> Json.Decode.Decoder a
decodeOpenUnion typeField types unknown =
    let
        decoders =
            Dict.fromList types
    in
    Json.Decode.field typeField Json.Decode.string
        |> Json.Decode.andThen
            (\type_ ->
                Dict.get type_ decoders
                    |> Maybe.withDefault (unknown type_)
            )


decodeUnion : String -> List ( String, Json.Decode.Decoder a ) -> Json.Decode.Decoder a
decodeUnion typeField types =
    let
        onFail type_ =
            Json.Decode.fail ("Unexpected " ++ typeField ++ ": " ++ type_)
    in
    decodeOpenUnion typeField types onFail
