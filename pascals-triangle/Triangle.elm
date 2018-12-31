module Triangle exposing (rows)

import Array exposing (Array)


rows : Int -> List (List Int)
rows n =
    if n < 1 then
        []

    else
        buildRow n |> List.reverse


buildRow : Int -> List (List Int)
buildRow n =
    if n == 1 then
        [ 1 ] :: []

    else
        let
            prevRow =
                n - 1 |> buildRow
        in
        buildRowItem n prevRow :: prevRow


buildRowItem : Int -> List (List Int) -> List Int
buildRowItem n prevRow =
    List.indexedMap
        (\i value ->
            if i == List.length prevRow - 1 then
                calculateValue i prevRow :: [ 1 ]

            else
                calculateValue i prevRow :: []
        )
        prevRow
        |> List.concat


calculateValue : Int -> List (List Int) -> Int
calculateValue i list =
    let
        array =
            list
                |> List.concat
                |> Array.fromList

        prevValue =
            getValue (i - 1) array

        value =
            getValue i array
    in
    prevValue + value


getValue : Int -> Array Int -> Int
getValue i array =
    array
        |> Array.get i
        |> Maybe.withDefault 0
