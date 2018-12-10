module Anagram exposing (detect)


detect : String -> List String -> List String
detect =
    isAnagram >> List.filter


isAnagram : String -> String -> Bool
isAnagram word candidate =
    String.toLower candidate
        /= String.toLower word
        && formatWord candidate
        == formatWord word


formatWord : String -> String
formatWord =
    String.toLower >> String.toList >> List.sort >> String.fromList
