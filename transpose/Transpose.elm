module Transpose exposing (transpose)


transpose : List String -> List String
transpose lines =
    let
        maxLength =
            lines
                |> List.map String.length
                |> List.maximum
                |> Maybe.withDefault 0
    in
    List.range 0 (maxLength - 1)
        |> List.map
            (\i ->
                List.foldl
                    (\( j, string ) acc ->
                        if i >= String.length string && shouldAddPadding i j lines then
                            acc ++ " "

                        else
                            acc ++ String.slice i (i + 1) string
                    )
                    ""
                    (List.indexedMap Tuple.pair lines)
            )


shouldAddPadding : Int -> Int -> List String -> Bool
shouldAddPadding i j lines =
    List.foldl
        (\( k, string ) acc ->
            if k > j && String.length string > i then
                True

            else
                acc
        )
        False
        (List.indexedMap Tuple.pair lines)
