module Sublist exposing (ListComparison(..), sublist)


type ListComparison
    = Equal
    | Superlist
    | Sublist
    | Unequal


sublist : List a -> List a -> ListComparison
sublist alist blist =
    if alist == blist then
        Equal

    else if isSubList alist blist then
        Sublist

    else if isSubList blist alist then
        Superlist

    else
        Unequal


isSubList : List a -> List a -> Bool
isSubList alist blist =
    if List.length alist > List.length blist then
        False

    else if List.take (List.length alist) blist == alist then
        True

    else
        isSubList alist (List.drop 1 blist)
