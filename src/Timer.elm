module Timer exposing (advance)

import Millis exposing (Millis)


advance : Millis -> Millis -> Maybe { a | elapsed : Millis } -> Maybe { a | elapsed : Millis }
advance lifetime delta timer =
    case timer of
        Nothing ->
            Nothing

        Just current ->
            let
                elapsed =
                    current.elapsed + delta
            in
            if elapsed > lifetime then
                Nothing

            else
                Just { current | elapsed = elapsed }
