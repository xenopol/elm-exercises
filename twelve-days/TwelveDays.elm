module Main exposing (getLyrics, recite)

import Array


getLyrics : Int -> Int -> List String -> List String
getLyrics start stop lyrics =
    let
        days =
            Array.fromList
                [ "first"
                , "second"
                , "third"
                , "fourth"
                , "fifth"
                , "sixth"
                , "seventh"
                , "eighth"
                , "nineth"
                , "tenth"
                , "eleventh"
                , "twelfth"
                ]

        beginning =
            "On the " ++ currentDay ++ " day of Christmas my true love gave to me, "

        linkWord =
            if stop > 1 then
                "and "

            else
                ""

        items =
            Array.fromList
                [ linkWord ++ "a Partridge in a Pear Tree"
                , "two Turtle Doves, "
                , "three French Hens, "
                , "four Calling Birds, "
                , "five Gold Rings, "
                , "six Geese-a-Laying, "
                , "seven Swans-a-Swimming, "
                , "eight Maids-a-Milking, "
                , "nine Ladies Dancing, "
                , "ten Lords-a-Leaping, "
                , "eleven Pipers Piping, "
                , "twelve Drummers Drumming, "
                ]

        currentDay =
            Maybe.withDefault "" (Array.get (start - 1) days)

        currentItem =
            Maybe.withDefault "" (Array.get (start - 1) items)
    in
    if start >= stop then
        ("."
            |> (++) (String.concat lyrics)
            |> (++) currentItem
            |> (++) beginning
        )
            :: []

    else
        getLyrics
            (start + 1)
            stop
            (currentItem :: lyrics)


recite : Int -> Int -> List String
recite start stop =
    getLyrics start stop []
