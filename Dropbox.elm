module Dropbox (client) where

import Native.Dropbox

type alias Client =
  { read: String -> Signal String
  , write: String -> Signal String -> ()
  }

client : String -> Client
client = Native.Dropbox.client
