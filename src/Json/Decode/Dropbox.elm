module Json.Decode.Dropbox exposing (optional, tagObject, tagValue, tagVoid)

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
