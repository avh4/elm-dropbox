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
    , headers = []
    , url = "https://api-content.dropbox.com/1/files_put/auto/test.txt"
    , body = Http.string "Hello"
    }

result = Signal.mailbox Maybe.Nothing

port runner : Task.Task Http.RawError ()
port runner = writeFile
    `Task.andThen` (Maybe.Just >> Signal.send result.address)

main = Signal.map (toString >> Html.text) result.signal