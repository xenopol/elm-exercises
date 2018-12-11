module RunLengthEncoding exposing (decode, encode)

-- Encoding


encodeWithDash : Char -> String -> String
encodeWithDash char acc =
    let
        letter =
            String.fromChar char
    in
    if String.endsWith letter acc then
        acc ++ letter

    else
        acc ++ "-" ++ letter


encodeString : String -> String -> String
encodeString group acc =
    let
        groupLength =
            String.length group

        groupChar =
            String.slice 0 1 group

        count =
            if groupLength > 1 then
                (groupLength |> String.fromInt) ++ groupChar

            else
                groupChar
    in
    acc ++ count


encode : String -> String
encode =
    String.foldl encodeWithDash ""
        >> String.split "-"
        >> List.filter (String.isEmpty >> not)
        >> List.foldl encodeString ""



-- Decoding


decodeWithDash : Char -> String -> String
decodeWithDash char acc =
    let
        letter =
            String.fromChar char
    in
    if Char.isAlpha char || Char.toCode char == 32 then
        acc ++ letter ++ "-"

    else
        acc ++ letter


decodeString : String -> String -> String
decodeString group acc =
    let
        letter =
            String.right 1 group

        count =
            String.slice 0 -1 group |> String.toInt
    in
    if String.length group > 1 then
        acc ++ String.repeat (Maybe.withDefault 0 count) letter

    else
        acc ++ letter


decode : String -> String
decode =
    String.foldl decodeWithDash ""
        >> String.split "-"
        >> List.filter (String.isEmpty >> not)
        >> List.foldl decodeString ""
