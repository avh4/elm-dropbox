module Dropbox (read) where

import Native.Dropbox

-- apiKey, filename
read : String -> String -> Signal String
read = Native.Dropbox.read
