module BinarySearch exposing (find)

import Array exposing (Array)


find : Int -> Array Int -> Int
find target xs =
    findElement target 0 xs


findElement : Int -> Int -> Array Int -> Int
findElement target index xs =
    let
        half =
            Array.length xs // 2
    in
    case Array.get half xs of
        Nothing ->
            -1

        Just item ->
            case compare target item of
                EQ ->
                    half + index

                LT ->
                    Array.slice 0 half xs
                        |> findElement target index

                GT ->
                    Array.slice (half + 1) (Array.length xs) xs
                        |> findElement target (index + half + 1)
