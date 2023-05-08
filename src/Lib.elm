module Lib exposing
    ( Triangle
    , TriangleType(..)
    , doubleFirst1
    , doubleFirst2
    , filterTrianglesByType
    , filterValidTriangles
    , indexOf
    , makeTriangles
    , parseValues
    , sortTrianglesByArea
    , sumTrianglesArea
    )

import Dict exposing (Dict)
import List.Extra as ListExtra
import Regex


{-| Represents a triangle with sides a, b, c.
-}
type alias Triangle =
    { a : Float
    , b : Float
    , c : Float
    }


{-| Defines the type of a triangle
-}
type TriangleType
    = RightAngled
    | Isosceles
    | Equilateral


{-| Write a function that lists of sides (as, bs and cs) and creates a list of
triangles from those sides.
-}
makeTriangles : List Float -> List Float -> List Float -> List Triangle
makeTriangles =
    List.map3 Triangle


{-| Write a function that takes a list of triangles and returns a list of only
valid triangles.
-}
filterValidTriangles : List Triangle -> List Triangle
filterValidTriangles =
    List.filter isValid


isValid : Triangle -> Bool
isValid triangle =
    let
        { a, b, c } =
            triangle
    in
    a + b > c && a + c > b && b + c > a && a > 0 && b > 0 && c > 0


{-| Write a function that takes a triangle type and a list of triangles and
returns a list of triangles that conforms the given type.
-}
filterTrianglesByType : TriangleType -> List Triangle -> List Triangle
filterTrianglesByType triangleType =
    List.filter
        (sortLongestSide
            >> (\triangle ->
                    let
                        filterByType =
                            case triangleType of
                                RightAngled ->
                                    isRightAngled

                                Isosceles ->
                                    isIsosceles

                                Equilateral ->
                                    isEquilateral
                    in
                    isValid triangle && filterByType triangle
               )
        )


sortLongestSide : Triangle -> Triangle
sortLongestSide triangle =
    let
        { a, b, c } =
            triangle
    in
    if c > b && c > a then
        triangle

    else if b > c && b > a then
        { a = a
        , b = c
        , c = b
        }

    else
        { a = c
        , b = b
        , c = a
        }


isRightAngled : Triangle -> Bool
isRightAngled { a, b, c } =
    a ^ 2 + b ^ 2 == c ^ 2


isIsosceles : Triangle -> Bool
isIsosceles { a, b, c } =
    a == b || b == c


isEquilateral : Triangle -> Bool
isEquilateral { a, b, c } =
    a == b && b == c


{-| Write a function that takes a list of triangles and returns a list of
triangles sorted by their area.
-}
sortTrianglesByArea : List Triangle -> List Triangle
sortTrianglesByArea =
    List.sortBy triangleArea


triangleArea : Triangle -> Float
triangleArea { a, b, c } =
    let
        s =
            (a + b + c) / 2
    in
    sqrt (s * (s - a) * (s - b) * (s - c))


{-| Write a function that takes a list of triangles and returns their area
-}
sumTrianglesArea : List Triangle -> Float
sumTrianglesArea =
    List.map triangleArea >> List.sum


{-| Write a function that has argument of type Maybe (List Int) and returns
the first number of the list mulitplied by 2.
If the list is Nothing or empty, it should return 0.

Do not use pattern matching or conditions, use helper functions from Maybe
package and |> operator to chain them together.

-}
doubleFirst1 : Maybe (List Int) -> Int
doubleFirst1 maybeList =
    maybeList
        |> Maybe.withDefault []
        |> List.head
        |> Maybe.withDefault 0
        |> (*) 2


{-| Write a function that works in the same way as doubleFirst1 but use point free
style and >> or << operators to chain needed functions together.
-}
doubleFirst2 : Maybe (List Int) -> Int
doubleFirst2 =
    Maybe.withDefault []
        >> List.head
        >> Maybe.withDefault 0
        >> (*) 2


doubleFirst2Alternative : Maybe (List Int) -> Int
doubleFirst2Alternative =
    Maybe.map (List.head >> Maybe.withDefault 0 >> (*) 2)
        >> Maybe.withDefault 0


{-| Write a function that takes a string that contains property names and
float values e.g.:

""""
a = 12.5
b = 5.4

tx = 18
"""

It converts the string into a dict with string keys and float values. Empty
lines and whitespaces should be skipped. The return value should be wrapped in
result and it should return an error if it cannot be parsed.

-}
parseValues : String -> Result String (Dict String Float)
parseValues str =
    let
        updateResult { match } resultDict =
            let
                splittedString =
                    match
                        |> String.replace " " ""
                        |> String.split "="
            in
            case splittedString of
                [ key, value ] ->
                    value
                        |> String.toFloat
                        |> Maybe.map
                            (\floatValue ->
                                resultDict
                                    |> Result.andThen (Dict.insert key floatValue >> Ok)
                            )
                        |> Maybe.withDefault (Err "Cannot parse string to float")

                _ ->
                    resultDict
    in
    str
        |> String.replace "\t" ""
        |> String.replace "\n" ""
        |> Regex.find parseRegex
        |> List.foldl updateResult (Ok Dict.empty)


parseRegex : Regex.Regex
parseRegex =
    "[a-z]+ *= *[a-zA-Z0-9]+(.[0-9]+)?"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| This function should return an index of the given element in the list. If
the element does not exist, it should return Nothing.

Do not implement this function by yourself. Find an Elm package that
already has the function and use it here.

-}
indexOf : a -> List a -> Maybe Int
indexOf =
    ListExtra.elemIndex
