module GradeSchool exposing (addStudent, allStudents, empty, studentsInGrade)

import Dict exposing (Dict)


type alias Grade =
    Int


type alias Student =
    String


type alias School =
    Dict Int (List Student)


empty : School
empty =
    Dict.empty


addStudent : Grade -> Student -> School -> School
addStudent grade student school =
    Dict.insert
        grade
        (studentsInGrade grade school |> (::) student |> List.sort)
        school


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    Dict.get grade school |> Maybe.withDefault []


allStudents : School -> List ( Grade, List Student )
allStudents =
    Dict.toList
