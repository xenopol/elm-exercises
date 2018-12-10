module ScrabbleScore exposing (scoreWord)


type alias Score =
    ( Int, List Char )


scores : List Score
scores =
    [ ( 1, [ 'a', 'e', 'i', 'o', 'u', 'l', 'n', 'r', 's', 't' ] )
    , ( 2, [ 'd', 'g' ] )
    , ( 3, [ 'b', 'c', 'm', 'p' ] )
    , ( 4, [ 'f', 'h', 'v', 'w', 'y' ] )
    , ( 5, [ 'k' ] )
    , ( 8, [ 'j', 'x' ] )
    , ( 10, [ 'q', 'z' ] )
    ]


scoreWord : String -> Int
scoreWord x =
    String.foldl (calculateScore scores) 0 x


calculateScore : List Score -> Char -> Int -> Int
calculateScore scoreList letter acc =
    getScore letter scoreList |> (+) acc


getScore : Char -> List Score -> Int
getScore letter scoreList =
    case filterScoreList letter scoreList |> List.head of
        Just score ->
            Tuple.first score

        Nothing ->
            0


filterScoreList : Char -> List Score -> List Score
filterScoreList letter scoreList =
    List.filter (letter |> Char.toLower >> isMember) scoreList


isMember : Char -> Score -> Bool
isMember letter ( _, letters ) =
    List.member letter letters
