module Geometry exposing (Position, Rect, Screen, Vector)

-- GEOMETRY


type alias Position =
    ( Float, Float )


type alias Vector =
    ( Float, Float )


type alias Screen =
    { width : Float
    , height : Float
    }


type alias Rect =
    { left : Float
    , top : Float
    , right : Float
    , bottom : Float
    }
