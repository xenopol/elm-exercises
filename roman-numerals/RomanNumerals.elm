module RomanNumerals exposing (toRoman)


toRoman : Int -> String
toRoman number =
    if number > 3000 then
        "Number is larger than 3000"

    else
        number
            |> String.fromInt
            |> String.foldl
                (\l ( bracket, romanNumber ) ->
                    let
                        c =
                            l |> String.fromChar |> String.toInt |> Maybe.withDefault 0

                        numeral =
                            buildNumeral c bracket
                    in
                    ( bracket // 10, romanNumber ++ numeral )
                )
                ( getBracket number, "" )
            |> Tuple.second


numerals : List ( Int, String )
numerals =
    [ ( 1, "I" )
    , ( 5, "V" )
    , ( 10, "X" )
    , ( 50, "L" )
    , ( 100, "C" )
    , ( 500, "D" )
    , ( 1000, "M" )
    ]



-- get the bracket in which the number is enclosed
-- e.g.: 7 -> 1 (single digits), 92 -> 10 (tens), 231 -> 100 (hundreds), 4388 -> 1000 (thousands)


getBracket : Int -> Int
getBracket =
    String.fromInt
        >> String.length
        >> (\l -> "1" ++ String.repeat (l - 1) "0")
        >> String.toInt
        >> Maybe.withDefault 0


getNumeralFromBracket : Int -> String
getNumeralFromBracket bracket =
    List.foldl
        (\( i, v ) acc ->
            if i == bracket then
                acc ++ v

            else
                acc
        )
        ""
        numerals


buildNumeral : Int -> Int -> String
buildNumeral number bracket =
    List.foldr
        (\( currentBracket, currentNumeral ) acc ->
            let
                bracketNumeral =
                    getNumeralFromBracket bracket
            in
            if number == 9 && bracket * 10 == currentBracket then
                acc ++ bracketNumeral ++ currentNumeral

            else if number < 9 && number > 5 && bracket * 5 == currentBracket then
                acc ++ currentNumeral ++ String.repeat (number - 5) bracketNumeral

            else if number == 5 && bracket * 5 == currentBracket then
                acc ++ currentNumeral

            else if number == 4 && bracket * 5 == currentBracket then
                acc ++ bracketNumeral ++ currentNumeral

            else if number < 4 && number > 0 && bracket == currentBracket then
                acc ++ String.repeat number currentNumeral

            else
                acc
        )
        ""
        numerals
