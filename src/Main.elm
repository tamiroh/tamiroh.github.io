module Main exposing (main)

import Browser
import Html exposing (Html)


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , update = \_ model -> model
        , view = view
        }


view : () -> Html ()
view _ =
    Html.text ""
