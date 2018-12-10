module NucleotideCount exposing (nucleotideCounts)


type alias NucleotideCounts =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }


nucleotideCounts : String -> NucleotideCounts
nucleotideCounts sequence =
    let
        emptyNucleotide =
            NucleotideCounts 0 0 0 0
    in
    case sequence of
        "" ->
            emptyNucleotide

        _ ->
            String.foldl
                (\c acc ->
                    case Char.toLower c of
                        'a' ->
                            { acc | a = acc.a + 1 }

                        't' ->
                            { acc | t = acc.t + 1 }

                        'c' ->
                            { acc | c = acc.c + 1 }

                        'g' ->
                            { acc | g = acc.g + 1 }

                        _ ->
                            emptyNucleotide
                )
                emptyNucleotide
                sequence
