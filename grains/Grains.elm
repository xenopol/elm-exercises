module Grains exposing (square)


square : Int -> Maybe Int
square n =
    if n > 0 then
        n - 1 |> (^) 2 |> Just

    else
        Nothing
