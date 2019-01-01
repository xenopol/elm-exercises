module BracketPush exposing (isPaired)


isPaired : String -> Bool
isPaired input =
    if
        hasPair "(" ")" input
            && hasPair "[" "]" input
            && hasPair "{" "}" input
            && isNestedCorrectly input
    then
        True

    else
        False


hasPair : String -> String -> String -> Bool
hasPair open closed input =
    let
        openIndexes =
            String.indexes open input

        closedIndexes =
            String.indexes closed input
    in
    if
        List.length openIndexes == List.length closedIndexes
            && hasCorrectOrder openIndexes closedIndexes
    then
        True

    else
        False


hasCorrectOrder : List Int -> List Int -> Bool
hasCorrectOrder open closed =
    List.map2 (<) open closed
        |> List.all identity


isNestedCorrectly : String -> Bool
isNestedCorrectly input =
    String.foldl
        (\c acc ->
            let
                s =
                    String.fromChar c
            in
            if s == "(" then
                acc ++ ")"
            else if s == "[" then
                acc ++ "]"
            else if s == "{" then
                acc ++ "}"
            else if s == ")" then
                updateAcc s acc
            else if s == "]" then
                updateAcc s acc
            else if s == "}" then
                updateAcc s acc
            else
                acc
        )
        ""
        input
        |> String.length
        |> (==) 0


updateAcc : String -> String -> String
updateAcc c acc =
    if c == String.dropLeft (String.length acc - 1) acc then
        String.dropRight 1 acc
    else
        acc
