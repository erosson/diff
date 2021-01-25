port module Ports exposing (..)

import Json.Decode as Json


port diffRequest : Json.Value -> Cmd msg


port diffResponse : (Json.Value -> msg) -> Sub msg
