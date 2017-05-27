module Json.Decode.Dropbox exposing (optional)

import Json.Decode exposing (..)
import Json.Decode.Pipeline as Pipeline


optional : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
optional field decoder =
    Pipeline.optional field (nullable decoder) Nothing
