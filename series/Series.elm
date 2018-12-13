module Series exposing (slices)


slices : Int -> String -> Result String (List (List Int))
slices size input =
    if String.isEmpty input then
        Err "series cannot be empty"

    else if size > String.length input then
        Err "slice length cannot be greater than series length"

    else if size == 0 then
        Err "slice length cannot be zero"

    else if size < 0 then
        Err "slice length cannot be negative"

    else
        convertStringToListOfInt input |> getSlices size |> Ok


convertStringToListOfInt : String -> List Int
convertStringToListOfInt =
    String.toList
        >> List.map (String.fromChar >> String.toInt >> Maybe.withDefault 0)


getSlices : Int -> List Int -> List (List Int)
getSlices size input =
    if List.length input >= size then
        List.take size input :: getSlices size (List.drop 1 input)

    else
        []
