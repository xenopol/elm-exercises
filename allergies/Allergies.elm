module Allergies exposing (isAllergicTo, toList)


allergenFlags : List ( String, Int )
allergenFlags =
    [ ( "eggs", 1 )
    , ( "peanuts", 2 )
    , ( "shellfish", 4 )
    , ( "strawberries", 8 )
    , ( "tomatoes", 16 )
    , ( "chocolate", 32 )
    , ( "pollen", 64 )
    , ( "cats", 128 )
    ]


hasFlag : Int -> ( a, Int ) -> Bool
hasFlag score ( _, flag ) =
    modBy (flag * 2) score >= flag


isAllergicTo : String -> Int -> Bool
isAllergicTo name score =
    allergenFlags
        |> List.filter (Tuple.first >> (==) name)
        |> List.any (hasFlag score)


toList : Int -> List String
toList score =
    allergenFlags
        |> List.filter (hasFlag score)
        |> List.map Tuple.first
