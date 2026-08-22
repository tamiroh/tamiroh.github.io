module Field exposing (Field, fits)

import Geometry exposing (Position, Screen)
import Obstacle exposing (Obstacle)



-- FIELD


type alias Field =
    { screen : Screen
    , objects : List Obstacle
    }


fits : Float -> Field -> Position -> Bool
fits margin field point =
    List.all
        (\object -> not (Obstacle.contains (Obstacle.grow margin object) point))
        field.objects
