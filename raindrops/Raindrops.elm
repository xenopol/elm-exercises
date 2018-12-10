module Raindrops exposing (raindrops)


getSounds : Int -> ( Int, String ) -> String -> String
getSounds number ( factor, sound ) acc =
    if remainderBy factor number == 0 then
        acc ++ sound

    else
        acc


raindrops : Int -> String
raindrops number =
    let
        factors =
            [ ( 3, "Pling" ), ( 5, "Plang" ), ( 7, "Plong" ) ]

        sounds =
            List.foldl (getSounds number) "" factors
    in
    if String.isEmpty sounds then
        String.fromInt number

    else
        sounds
