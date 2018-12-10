module RNATranscription exposing (toRNA)


convert : Char -> Char
convert letter =
    case letter of
        'G' ->
            'C'

        'C' ->
            'G'

        'T' ->
            'A'

        'A' ->
            'U'

        _ ->
            'E'


toRNA : String -> Result Char String
toRNA dna =
    let
        rna = String.map convert dna
    in
    if String.any (\c -> c == 'E') rna then
        Err 'E'
    else
        Ok rna

