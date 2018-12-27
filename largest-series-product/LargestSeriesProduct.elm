module LargestSeriesProduct exposing (largestProduct)


largestProduct : Int -> String -> Maybe Int
largestProduct length series =
    let
        seriesLength =
            String.length series
    in
    if
        (String.all Char.isDigit series |> not)
            || length
            > seriesLength
            || length
            < 0
    then
        Nothing

    else if length == 0 && seriesLength == 0 then
        Just 1

    else
        series
            |> String.toList
            |> List.filterMap (String.fromChar >> String.toInt)
            |> getLargest length
            |> List.map List.product
            |> List.maximum


getLargest : Int -> List Int -> List (List Int)
getLargest length numbers =
    if List.length numbers >= length then
        case numbers of
            [] ->
                []

            head :: tail ->
                List.take length numbers :: getLargest length tail

    else
        []
