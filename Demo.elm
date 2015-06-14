import Html
import Http
import Task
import Maybe

type alias AuthToken = String

authToken : AuthToken
authToken = "0Duxz7GkOpEAAAAAAAAN-qiJaUsrvNWIfDs0CGzLiKm6V1VGW0OLDc7dGy3SePeb"

host : String
host = "https://api-content.dropbox.com/1/files_put/auto/test.txt"

writeFile : Task.Task Http.RawError Http.Response
writeFile = Http.send Http.defaultSettings
    { verb = "PUT"
    , headers = [("Authorization", "Bearer " ++ authToken)]
    , url = "https://api-content.dropbox.com/1/files_put/auto/test.txt"
    , body = Http.string "Hello"
    }

readFile : Task.Task Http.RawError Http.Response
readFile = Http.send Http.defaultSettings
    { verb = "GET"
    , headers = [("Authorization", "Bearer " ++ authToken)]
    , url = "https://api-content.dropbox.com/1/files/auto/test.txt"
    , body = Http.empty
    }

actionBox = Signal.mailbox <| ContentLoaded ""

port runner : Task.Task Http.RawError ()
port runner = writeFile
    `Task.andThen` (WriteStatus >> Signal.send actionBox.address)

port runner2 : Task.Task Http.RawError ()
port runner2 = readFile
    `Task.andThen` (.value >> toString >> ContentLoaded >> Signal.send actionBox.address)

init =
    { content = ""
    , writeStatus = Nothing
    }

type Action
    = ContentLoaded String
    | WriteStatus Http.Response

step action model = case action of
    ContentLoaded s -> { model | content <- s }
    WriteStatus r -> { model | writeStatus <- Just r }

actions = Signal.mergeMany
    [ actionBox.signal
    ]

model = Signal.foldp step init actions

input = Html.input [] []
main = Signal.map render model

render m =
    Html.div []
    [ Html.input [] []
    , Html.hr [] []
    , Html.div [] [Html.text m.content]
    , Html.hr [] []
    , Html.div [] [Html.text <| toString m.writeStatus]
    ]